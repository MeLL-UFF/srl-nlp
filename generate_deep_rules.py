#!/bin/env python
from srl_nlp.framenet.parse_xml import NetXMLParser
from srl_nlp.rule_manipulation  import *
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


class RuleGenerator(object):
    '''Take Examples and convert them to rules'''
    FE_PRED = 'frame_element'
    FR_PRED = 'frame_related'

    def __init__(self, *analysers, **params):
        self.analysers = analysers
        nlp = spacy.load('en_core_web_sm')
        self._get_lemma = lambda token: nlp(token.decode('utf-8'))[0].lemma_

    def get_rules(self, frame, *examples):
        '''Generates FrameElement rules'''
        for analyser in self.analysers:
            for example in examples:
                lfs = analyser.sentence2LF(example.str_no_annotation())
                for lf in lfs:
                    if not lf.has_pred('not'):
                        logger.debug('Path:%s' %str(lf))
                        try:
                            elements, targets = get_annotations(example, lf, get_abbrev(frame), get_lemma = self._get_lemma)
                            logger.debug('Done Annotations:%s' %str(targets))
                            factors = get_factors(lf)
                            logger.debug('Done Factors:%s' %str(factors))
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
                                            rule = "%s :- %s, %s." %(str_preds(make_pred(self.FE_PRED, pred, label.lower())),
                                                                     str_preds(make_pred(self.FR_PRED, target, frame.name.lower())),
                                                                     str_preds(path))
                                            logger.debug("Rule: %s" %rule)
                                            yield rule
                                            break
                        except IndexError as e:
                            logger.error(e)
                            continue


def make_theory(rule_generator, fe_examples):
    '''
    Returns a iterator of strings where each string is a prolog rule.
    '''
    for i in fe_examples:
        examples, frame = i
        logger.debug('example | frame:  %s'%str(i))
        if len(examples) > 0:
            logger.info('Example: %s' %str(examples[0].str_no_annotation()))
        for rule in rule_generator.get_rules(frame, *examples):
            yield rule
    logger.info('Made theory')



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
    parser = argparse.ArgumentParser(description = 'KB generator')
    parser.add_argument('--out_file', help = 'the path to where to write the rules')
    parser.add_argument('--example_file', help = 'the path to where to write/read examples TODO')
    parser.add_argument('-l', '--limit', help = 'max number of examples used')
    parser.add_argument('-r', '--frame_related', action='store_true', default = False,
                        help = 'output frame_related rules')
    parser.add_argument('-e', '--frame_element', action='store_true', default = False,
                        help = 'output frame_element rules')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
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

    # lfs = None
    # overwrite_lfs_file = False
    # if args.lfs_file:
    #     logger.info('Reading from %s' %args.lfs_file)
    #     try:
    #         with open(args.lfs_file, 'r') as f:
    #             lfs = pickle.load(f)
    #     except IOError:
    #         overwrite_lfs_file = True
    # if not lfs:
    #     lfs = dict()
    #     for count, (fe, example_list) in enumerate(examples.iteritems()):
    #         lfs[fe] = [boxer.sentence2LF(example.str_no_annotation())
    #                         for ex_list, _ in example_list for example in ex_list]
    #         logger.info("%06.2f%%" %(100.*(count+1)/len(examples)))
    #     if overwrite_lfs_file:
    #         with open(args.lfs_file, 'w') as f:
    #             pickle.dump(lfs, f)

    # logger.info('LFs are ready')

    if args.frame_related:
        frame_matching_rules = make_frame_matching_rules(get_lus2frames(fn))

        if args.out_file:
            logger.info('Saving to %s' %args.out_file)
            stream = open(args.out_file, 'a')
        else:
            stream = stdout
        with stream as f:
            f.write("%% Frame Matching Rules\n")
            for rule in frame_matching_rules:
                f.write(rule)
                f.write('\n')

    ########################
    #                      #
    #  Frame Element Rules #
    #                      #
    ########################
    if args.frame_element:
        fes_keys = fn._fes.keys()
        examples = []
        if args.example_file:
            try:
                with open(args.example_file, 'r') as f:
                    pass
            except: #TODO Not found error
                pass
        else:
            logger.info('Capturing examples')
            examples = []
            for l in list(map(lambda fe: get_examples(fe,fn), fes_keys)):
                examples.extend(l)

        logger.info('Starting Boxer')
        boxer = BoxerLocalAPI(expand_predicates = True)
        logger.info('Boxer started')
        if args.limit:
            sliced_examples = examples[:int(args.limit)]
        else:
            sliced_examples = examples
        logger.info('Ther are %d examples being considered' %len(sliced_examples))
        theory = make_theory(RuleGenerator(boxer), sliced_examples)
            
        if args.out_file:
            logger.info('Saving to %s' %args.out_file)
            stream = open(args.out_file, 'a')
        else:
            stream = stdout
        with stream as f:
            f.write("\n%% Deep Role Rules\n")
            for rule in theory:
                f.write(rule)
                f.write('\n')




if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)