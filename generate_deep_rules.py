from srl_nlp.framenet.parse_xml import NetXMLParser
from srl_nlp.framenet.framenet  import Description
from srl_nlp.analysers.boxer    import BoxerLocalAPI
from srl_nlp.logicalform        import LF
from srl_nlp.fol                import FOL
from regex                      import compile
from copy                       import deepcopy as copy
from sys                        import stdout
from os                         import path
import pickle
import spacy

#Logger
from ConfigParser    import ConfigParser
from logger_config   import config_logger, add_logger_args
from sys             import argv
import argparse
import logging


logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))

############################

#   Get Deep Role Rules    #

############################

def get_examples(target, fn):
    '''
        Iterates over the fn frames that contain target as a Frame element and return every description element with the tag EXample
    '''
    frames = fn.getFrameElementFrames(target)
    examples = []
    for frame in frames:
        fes = filter(lambda x: x == target, frame.coreFEs+frame.peripheralFEs)
        for fe in fes:
            examples.append((fe.definition.get_elements('ex'), frame))
    return examples


def get_preds(lf, token, skip = ['relation']):
    '''
    Return a list of predicates that contain the token
    '''
    out = []
    if not lf.get_pred() in skip:
        for term in lf.iterterms():
            if term.get_pred() == token:
                out.append(lf)
            else:
                out.extend(get_preds(term, token))
    return out


def get_abbrev(frame):
    '''
    Returns a dictionary mapping abbreviation to Frame Element name
    '''
    out = dict()
    for fe in frame.coreFEs+frame.peripheralFEs:
        if len(fe.abbrev) > 0:
            out[fe.abbrev] = fe.name
    return out

nlp = spacy.load('en_core_web_sm')


def get_annotations(example, lf, abbrev2fe = {}, get_lemma = lambda token: nlp(token.decode('utf-8'))[0].lemma_):
    '''
    Returns a tuple (fes_dict, taget_list), where
        fes_dict is a dictionary mapping Frame Element names to a predicate list
        target_list is a list of predicates that are target in this example
    '''
    fes = dict()
    target = []
    for term in example.content:
        pred_stack = []
        logger.debug("TERM %s" %term)
        logger.debug("Example %s" %example)
        while isinstance(term, Description.FEeXample) or \
              isinstance(term, Description.Target) or \
              isinstance(term, Description.T) :
            pred_stack.append(term.attribs.get('name', term.name))
            term = term.content[0]
        if len(pred_stack) > 0:
            for token in term.strip().split(' '):
                try:
                    if len(token) < 1:
                        continue
                    literals = get_preds(lf, get_lemma(token))
                except IndexError as e:
                    logger.debug("Term: '%s'" %term)
                    raise e
                for literal in literals:
                    for pred in pred_stack:
                        pred = abbrev2fe.get(pred, pred)
                        if pred =='target' or pred == 't':
                            target.append(literal)
                        elif pred == 'fex':
                            logger.error('Fex without attrib name')
                        else:
                            l = fes.get(pred, [])
                            l.append(literal)
                            fes[pred] = l
                            #if pred[:1].islower():
                            #    print "%% %s, '%s' '%s'\t %s" %(term, pred, pred_old, example.content)
    return fes, target


def get_factors(lf, out = None):
    '''
    Returns a mapping from the terms to pedicate lists
    '''
    if out == None:
        out = {}
    if FOL.is_operator(lf.get_pred()):
        for term in lf.iterterms():
            get_factors(term, out)
    else:
        for term in lf.iterterms():
            preds = out.get(term.get_pred(), [])
            preds.append(lf)
            out[term.get_pred()] = preds
    return out


def get_paths(predL, predR, factors, breadth = True):
    '''
    Given two predicates in LF, and a mapping of their literals given
    by 'get_factors' this function yields the paths that can link
    those predicates.
    '''
    frontier = [(predR,[])]
    visited = []
    while len(frontier):
        if breadth:
            curr_pred, path = frontier.pop(0)
        else:
            curr_pred, path = frontier.pop()
        if predL == curr_pred:
            yield path
        visited.append(curr_pred)
        for literal in curr_pred.iterterms():
            for term in factors.get(literal.get_pred(), []):
                if not term in visited:
                    frontier.append((term, path+[term]))

def make_pred(literal, pred, label):
    '''
    Returns a new LF predicate from the original pred, the literal and a label
    '''
    t = pred.iterterms().next()
    out = LF()
    out.info = [literal] + [t.info]+ [[label]]
    return out

def str_preds(preds, pattern = compile('^c(\d+)$'), x = ['frame_element', 'frame_related',
                                                        'relation'], count = None):
    '''
    Converts a LF or a list of LFs into a string in a convenient way to be rendered in a rule
        LF -> str
        [LF] -> str
    '''
    if not count:
        count = [0]
    def repl_const(match, count = count):
        count[0] = max(count[0], int(match.group(1)))
        return 'C'+match.group(1)

    def new_const(count = count):
        count[0] = count[0] +1
        return 'C%d' %count[0]

    generalize = []
    if isinstance(preds, LF):
        pred = copy(preds)
        out = pred.get_pred()
        for literal in pred.iterterms():
            if literal.isleaf():
                logger.debug("PRED::%s: update:%s" %(pred.get_pred(), not pred.get_pred() in x))
                if pattern.match(literal.get_pred()):
                    literal.set_pred(pattern.sub(repl_const, literal.get_pred()))
                elif not pred.get_pred() in x:
                    generalize.append(literal)
        for literal in generalize:
            literal.set_pred(new_const())
        return pred.__repr__(final_dot = False)
    else:
        return ','.join(map(lambda pred: str_preds(pred, pattern, x, count), preds))

def make_theory(lfs, examples):
    '''
    Returns a set of strings where each string is a prolog rule of a theory.
    '''
    theory = []
    for fe in lfs.iterkeys():
        out = []
        for (example_list, frame), lf_list in zip(examples[fe], lfs[fe]):
            for example in example_list:
                for lf in lf_list:
                    if lf.has_pred('not'):
                        continue
                    try:
                        elements, targets = get_annotations(example, lf, get_abbrev(frame))
                        factors = get_factors(lf)
                        for target in targets:
                            for label, preds in elements.iteritems():
                                for pred in preds:
                                    for path in get_paths(pred, target, factors):
                                        if label[:1].islower():
                                            logger.error(example)
                                            logger.error(path)
                                            logger.error(elements)
                                            logger.error(targets)
                                            raise Exception('%s FE was not matched, the label is lower cased' %label)
                                        theory.append("%s :- %s, %s." %(str_preds(make_pred('frame_element', pred, label.lower())),
                                                                str_preds(make_pred('frame_related', target, frame.name.lower())),
                                                                str_preds(path))
                                                     )
                                        break
                    except IndexError as e:
                        continue
    return theory



############################

# Get Frame Matching Rules #

############################

lu_pos2pred = {'A'    : 'adjective',
               # 'ADV'  : '', #adverb
               # 'ART'  : '', #quantity modifier
               # 'C'    : '', #conjuction
               # 'IDIO' : '', #idiom
               # 'INTJ' : '', #interjection
               'N'    : 'noun',
               'NUM'  : 'number',
               # 'PREP' : '', #preposition
               # 'PRON' : '', #pronoun
               # 'SCON' : '', #? conditional
               'V'    : 'verb'}

def get_lus2frames(frameNet):
    lus2frames = dict()
    for frame in frameNet:
        for lu in frame.LUs:
            s = lus2frames.get(lu, set())
            s.add(frame)
            lus2frames[lu] = s
    return lus2frames

def make_frame_matching_rules(lus2frames, lu_pos2pred = lu_pos2pred, f_out = None):
    out = []
    handle_name = lambda name: name.replace('-','_').replace(' ','_').lower()
    make_rule = lambda lu, frame: "frame_related(X, {frame})"\
                                  ":- {pos}(X, {token}).".format(frame = handle_name(frame.name),
                                                                 pos = lu_pos2pred[lu.pos],
                                                                 token = handle_name(lu.name))
    for lu in lus2frames.keys():
        for frame in lus2frames[lu]:
            try:
                if f_out:
                    f_out.write(make_rule(lu,frame))
                    f_out.write('\n')
                else:
                    out.append(make_rule(lu,frame))
            except KeyError as e:
                #logger.warning('Not supported part of speach tag of this LU: %s' %lu)
                pass
    if not f_out:
        return out



##########################

def parse_args(argv= argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Tmp file that generate the deep rules')
    parser.add_argument('--lfs_file', help = 'the path of file with the lfs (if the file does not exist, it will be created and filled)')
    parser.add_argument('--out_file', help = 'the path to where to write the rules')
    #parser.add_argument('-p', '--file_prefix', help = 'prefix of the experiment files')
    #parser.add_argument('-v', '--verbosity', action='count', default=0, help = 'increase output verbosity')
    add_logger_args(parser)
    args = parser.parse_args()
    return args

def main(argv):
    '''
    Process the arguments in order to return the theory.
    The theory is the union of the set of the deep role rulas and the
    set of the frame matching rules.
    '''
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    #logger.info(args)
    logger.info('Starting FrameNet')
    parser = NetXMLParser()
    fn = parser.parse('framenet/fndata-1.7')
    fes_keys = fn._fes.keys()
    logger.info('Capturing examples')
    examples = dict(zip(fes_keys, map(lambda fe: get_examples(fe,fn), fes_keys)))

    logger.info('Starting Boxer')
    boxer = BoxerLocalAPI(expand_predicates = True)
    logger.info('Boxer started')

    lfs = None
    overwrite_lfs_file = False
    if args.lfs_file:
        logger.info('Reading from %s' %args.lfs_file)
        try:
            with open(args.lfs_file, 'r') as f:
                lfs = pickle.load(f)
        except IOError:
            overwrite_lfs_file = True
    if not lfs:
        lfs = dict()
        for count, (fe, example_list) in enumerate(examples.iteritems()):
            lfs[fe] = [boxer.sentence2LF(example.str_no_annotation()) for example in example_list]
            logger.info("%06.2f%%" %(100.*(count+1)/len(examples)))
        if overwrite_lfs_file:
            with open(args.lfs_file, 'w') as f:
                pickle.dump(lfs, f)

    logger.info('LFs are ready')

    frame_matching_rules = make_frame_matching_rules(get_lus2frames(fn))

    if args.out_file:
        logger.info('Saving to %s' %args.out_file)
        with open(args.out_file, 'w') as f:
            f.write("%% Frame Matching Rules\n")
            for rule in frame_matching_rules:
                f.write(rule)
                f.write('\n')
    else:
        print "%% Frame Matching Rules"
        for rule in frame_matching_rules:
            print rule

    theory = make_theory(lfs, examples)
    if args.out_file:
        logger.info('Saving to %s' %args.out_file)
        with open(args.out_file, 'a') as f:
            f.write("\n%% Deep Role Rules\n")
            for rule in theory:
                f.write(rule)
                f.write('\n')
    else:
        print "\n%% Deep Role Rules"
        for rule in make_theory(lfs, examples):
            print rule




if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)