from requests             import post
from subprocess           import PIPE, Popen
from sys                  import stderr
from os                   import path
from tempfile             import TemporaryFile
from ConfigParser         import ConfigParser
from srl_nlp.fol          import FOL
from srl_nlp.logicalform  import LF
from regex                import match, compile
import json
import logging

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "../external.conf"))

class Process:
    '''Intended to be an abstract class
    Its subclasses must have an attribute "params" of the type list and a string attribute "path_to_bin"
    '''
    def __init__(self, path_to_bin, *params):
        self.path_to_bin = path_to_bin
        self.params      = params

    def _process(self, input_text, shell = False):
        with TemporaryFile() as tmp:
            tmp.write(input_text)
            tmp.seek(0)
            process = Popen([self.path_to_bin] + list(self.params),
                                       shell=shell,
                                       stdin=tmp,
                                       stdout=PIPE,
                                       stderr=PIPE)
            out, err = process.communicate()
        return out, err


class TokenizerLocalAPI(Process):
    def __init__(self, path_to_bin = config.get('syntatic_local', 't'), *params):
        if len(params) == 0:
            params = ('--stdin',)
        self.path_to_bin = path_to_bin
        self.params = params

    def tokenize(self, text):
        out, err = self._process(text)
        if err:
            print >> stderr, 'Tokenizer error: {0}'.format(err)
        tokenized = out.decode('utf-8').encode("utf-8")
        sentences = tokenized.split('\n')
        return [sentence.split(" ") for sentence in sentences]


class CandCLocalAPI(Process):
    def __init__(self, path_to_bin = config.get('semantic_local', 'c&c'),*params):
        if len(params) == 0:
            params = ('--models', config.get('semantic_local', 'c&c_models'), '--candc-printer', 'boxer')
        self.path_to_bin = path_to_bin
        self.params = params

    def parse(self, tokenized):
        tokenized = '\n'.join(map(' '.join, tokenized))
        out, err = self._process(tokenized)
        if err:
            # C&C writes info on the stderr, we want to ignore it
            if not err.startswith('#'):
                print >> stderr, 'Parser error: {0}'.format(err)
        return out.decode('utf-8').encode("utf-8")


class BoxerAbstract:
    '''Do not initialize this class. Use BoxerLocalAPI or BoxerWebAPI instead.
    '''
    _expansion_patterns = [
        #pattern:           lambda p_elems, terms: tuple([predicate, [term1], ..., [termN]],...)  
        (r'^pernam(\w*)',        lambda p_elems, terms: (['person']    + terms,
                                                        ['noun']       + terms + p_elems)),
        (r'^geonam\d?(\w*)',     lambda p_elems, terms: (['place']     + terms,
                                                        ['noun']       + terms + p_elems)),
        (r'^\w\d+(?:A|actor)',   lambda p_elems, terms: (['actor']     + terms,)),
        (r'^r\d+(?:T|t)heme',    lambda p_elems, terms: (['theme']     + terms,)),
        (r'^r\d+(?:T|t)opic',    lambda p_elems, terms: (['topic']     + terms,)),
        (r'^r\d+(\w*)',          lambda p_elems, terms: (['relation']  + terms + p_elems,)),
        (r'^n\d+numeral',        lambda p_elems, terms: (['numeral']   + terms,)),
        (r'^n\d+(.*)',           lambda p_elems, terms: (['noun']      + terms + p_elems,)),
        (r'^t_X+(\d+)',          lambda p_elems, terms: (['number']    + terms + p_elems,)),
        (r'^c(\d+)number',       lambda p_elems, terms: (['noun']      + terms + p_elems,)),
        (r'^c\d+numeral',        lambda p_elems, terms: (['numeral']   + terms,)),
        (r'^c\d+(.*)',           lambda p_elems, terms: (['cnoun']     + terms + p_elems,)),
        (r'^a\d+(.*)',           lambda p_elems, terms: (['adjective'] + terms + p_elems,)),
        (r'^v\d+c64placeholder', lambda p_elems, terms: (['action']    + terms,)),
        (r'^v\d+(.*)',           lambda p_elems, terms: (['verb']      + terms + p_elems,)),
    ]

    def __init__(self):
        'abstract class, do not use this method'
        assert True, 'You should not initialize this class'

    def sentence2FOL(self, sentence, *extra_args):
        parsed    = self._parse_sentence(sentence)
        boxed =  self._parsed2FOLstring(parsed)
        #print boxed
        #return lines that do not start with '%%%', nor 'id' and are not empty
        is_relevant = lambda x: not (x.startswith('id') or x.startswith('%%%')) and len(x) > 0
        raw_fols = filter(is_relevant, boxed.split("\n"))
        fols = map(lambda x: FOL(x, *extra_args), raw_fols)
        special_char_pattern = compile('C(\d+)')
        for fol in fols:
            frontier = [fol.info]
            while len(frontier):
                term = frontier.pop()
                term[0] = special_char_pattern.sub(lambda x: 'c%s' %x.group(1), term[0])
                frontier.extend(term[1:])
            logger.debug('Raw fol:',fol)
        for fol in fols:
            fol.info = fol.info[-1] #remove header
        return fols

    def FOL2LF(self, fol_list, expand_predicates, removeForAlls = True, **kargs):
        # print fol_list
        # raw_input()
        to_LF = lambda x: LF(x, removeForAlls=removeForAlls, header = 'fol',**kargs)
        if expand_predicates:
            parse = lambda x: to_LF(BoxerAbstract._expandFOLpredicates(x))
        else:
            parse = to_LF
        out = map(parse, fol_list)
        # print out
        # raw_input()
        return out

    @staticmethod
    def _expandFOLpredicate(fol):
        predicate = fol[0]
        args = fol[1:]
        #print predicate, '-', len(args), '\n\n\n'
        for pattern, parser in BoxerAbstract._expansion_patterns:
            matching = match(pattern, predicate)
            if matching:
                pred_elems = map(lambda x: [x.lower()], matching.groups())
                out = parser(pred_elems, list(args))
                logger.debug(' {exp} :- {fol}'.format(exp = out, fol = fol))
                return out
        return None

    @staticmethod
    def _expandFOLpredicates(fol, concatenator = FOL.AND):
        if fol == None:
            return None
        #print '\n\n expanding', fol
        frontier = [fol.info]
        while len(frontier) > 0:
            term = frontier.pop()
            predicate = term[0]
            #print '\ncheck:', predicate,'\n\n'
            if FOL.is_special(predicate):
                #print 'is special'
                for pos, child in enumerate(term[1:]):
                    expansion = BoxerAbstract._expandFOLpredicate(child)
                    if expansion:
                        if len(expansion) > 1:
                            replacement = [concatenator] #FOL.AND
                            chain_curr = replacement
                            for child_term in expansion[:-1]:
                                if len(chain_curr) >= 2:
                                    chain_curr.append([concatenator])
                                    chain_curr = chain_curr[-1]
                                chain_curr.append(child_term)
                            chain_curr.append(expansion[-1])
                            term[pos+1] = replacement
                        else:
                            term[pos+1] = expansion[0]

                    else:
                        frontier.append(child)
        #print ">>",fol
        return fol

    def sentence2LF(self, sentence, source = None, id = None, expand_predicates = None,**kargs):
        if expand_predicates == None:
            expand_predicates = self.expand_predicates
        if not (source == None or id == None):
            fol = self.sentence2FOL(sentence, source, id)
        else:
            fol = self.sentence2FOL(sentence)
        return self.FOL2LF(fol, expand_predicates, **kargs)


class BoxerLocalAPI(Process, BoxerAbstract):
    def __init__(self, tokenizer   = TokenizerLocalAPI(),
                 ccg_parser        = CandCLocalAPI(),
                 expand_predicates = True,
                 path_to_bin       = config.get('semantic_local', 'boxer'), *params):
        if len(params) == 0:
            params = ('--stdin', '--semantics', 'fol')
        self.name        = 'Boxer'
        self.path_to_bin = path_to_bin
        self.ccg_parser  = ccg_parser
        self.tokenizer   = tokenizer
        self.params      = params
        self.expand_predicates = expand_predicates

    def _parsed2FOLstring(self, parsed):
        out, err = self._process(parsed)
        if err:
            # Boxer throws a silly error every time (a bug), we want to ignore it
            if not "No source location" in err:
                print >> stderr, 'Boxer error: {0}'.format(err)
        return out.decode('utf-8').encode("utf-8")

    def _parse_sentence(self, sentence):
        tokenized = self.tokenizer.tokenize(sentence)
        parsed    = self.ccg_parser.parse(tokenized)
        return parsed


class BoxerWebAPI(BoxerAbstract):
    def __init__(self, url = config.get('semantic_soap', 'boxer'), expand_predicates = True):
        self.url = url
        self.name = 'boxer'
        self.expand_predicates = expand_predicates

    def _parse_sentence(self, sentence):
        return sentence

    def _parsed2FOLstring(self, parsed):
        return  post(self.url, data = parsed).text.strip()