import logging
from ConfigParser import ConfigParser
from abc import abstractmethod
from os import path
from sys import stderr

from process import Process
from regex import match, compile
from requests import post
from srl_nlp.fol import FOL
from srl_nlp.logicalform import LF

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "../external.conf"))

TIME_OUT = 100


class TokenizerLocalAPI(Process):
    def __init__(self, path_to_bin=config.get('syntatic_local', 't'), *params):
        if len(params) == 0:
            params = ('--stdin',)
        Process.__init__(self, path_to_bin, True, TIME_OUT, *params)

    def tokenize(self, text):
        out, err = self._process(text.strip())
        if err:
            print >> stderr, 'Tokenizer error: {0}'.format(err)
        tokenized = out.decode('utf-8').encode("utf-8")
        sentences = tokenized.split('\n')
        return [sentence.split(" ") for sentence in sentences]


class CandCLocalAPI(Process):
    def __init__(self, path_to_bin=config.get('semantic_local', 'c&c'), *params):
        if len(params) == 0:
            params = ('--models', config.get('semantic_local', 'c&c_models'), '--candc-printer', 'boxer')
        Process.__init__(self, path_to_bin, False, TIME_OUT, *params)

    def _header_completed(self, out_list):
        return sum(map(lambda x: (x == '\n'), out_list)) >= 2

    def _process_completed(self, out_list):
        opening = sum(map(lambda line: line.count('('), out_list))
        closing = sum(map(lambda line: line.count(')'), out_list))
        return opening == closing and sum(map(lambda x: (x == '\n'), out_list)) >= 1

    def parse(self, tokenized):
        out = ''
        for tokenized in map(' '.join, tokenized):
            tokenized = tokenized.strip()
            if len(tokenized) > 0:
                tmp_out, err = self._process(tokenized.strip() + '\n')
                if err:
                    # C&C writes info on the stderr, we want to ignore it
                    if not err.startswith('#'):
                        print >> stderr, 'Parser error: {0}'.format(err)
                out = out + tmp_out
        return out.decode('utf-8').encode("utf-8")


class BoxerAbstract:
    """
    Abstract Class

    Do not initialize this class. Use BoxerLocalAPI or BoxerWebAPI instead.
    """
    _expansion_patterns = [
        # pattern:           lambda p_elems, terms: tuple([predicate, [term1], ..., [termN]],...)
        (r'^pernam(\w*)', lambda p_elems, terms: (['person'] + terms,
                                                  ['noun'] + terms + p_elems)),
        (r'^namnam(\w*)', lambda p_elems, terms: (['person'] + terms,
                                                  ['noun'] + terms + p_elems)),
        (r'^orgnam(\w*)', lambda p_elems, terms: (['organization'] + terms,
                                                  ['noun'] + terms + p_elems)),
        (r'^geonam\d?(\w*)', lambda p_elems, terms: (['place'] + terms,
                                                     ['noun'] + terms + p_elems)),
        (r'^timnam\d?(\w*)', lambda p_elems, terms: (['time'] + terms + p_elems,)),
        (r'^\w\d+(?:A|actor)', lambda p_elems, terms: (['actor'] + terms,)),
        (r'^r\d+(?:T|t)heme', lambda p_elems, terms: (['theme'] + terms,)),
        (r'^\w\d+(?:T|t)opic', lambda p_elems, terms: (['topic'] + terms,)),
        (r'^r\d+(\w*)', lambda p_elems, terms: (['relation'] + terms + p_elems,)),
        (r'^n\d+numeral', lambda p_elems, terms: (['numeral'] + terms,)),
        (r'^n\d(.*)', lambda p_elems, terms: (['noun'] + terms + p_elems,)),
        (r'^t_X+(\d+)', lambda p_elems, terms: (['number'] + terms + p_elems,)),
        (r'^c(\d+)number', lambda p_elems, terms: (['noun'] + terms + p_elems,)),
        (r'^c\d+numeral', lambda p_elems, terms: (['numeral'] + terms,)),
        (r'^c\d+(.*)', lambda p_elems, terms: (['cnoun'] + terms + p_elems,)),
        (r'^a\d+(.*)', lambda p_elems, terms: (['adjective'] + terms + p_elems,)),
        (r'^v\d+c64placeholder', lambda p_elems, terms: (['action'] + terms,)),
        (r'^v\d+(.*)', lambda p_elems, terms: (['verb'] + terms + p_elems,)),
    ]

    @abstractmethod
    def __init__(self):
        """abstract class, do not use this method"""
        self.expand_predicates = False
        # assert True, 'You should not initialize this class'

    @abstractmethod
    def _parse_sentence(self, sentence):
        return None

    @abstractmethod
    def _parsed2FOLstring(self, parsed):
        return None

    def sentence2FOL(self, sentence, *extra_args):
        parsed = self._parse_sentence(sentence)
        boxed = self._parsed2FOLstring(parsed)

        # print boxed
        # return lines that do not start with '%%%', nor 'id' and are not empty

        def is_relevant(x):
            return not (x.startswith('id') or x.startswith('%%%')) and len(x) > 0

        raw_fols = filter(is_relevant, boxed.split("\n"))
        fols = map(lambda x: FOL(x, *extra_args), raw_fols)
        special_char_pattern = compile('C(\d+)')
        for fol in fols:
            frontier = [fol.info]
            while len(frontier):
                term = frontier.pop()
                term[0] = special_char_pattern.sub(lambda x: 'c%s' % x.group(1), term[0])
                frontier.extend(term[1:])
            logger.debug('Raw fol: %s', fol)
        for fol in fols:
            fol.info = fol.info[-1]  # remove header
        return fols

    def FOL2LF(self, fol_list, expand_predicates, removeForAlls=True, remove_eq=True, **kwargs):
        # raw_input()
        def to_lf(fol, rem_eq, expand_pred):
            lf = LF(fol, removeForAlls=removeForAlls, header='fol', **kwargs)
            if expand_pred:
                lf = BoxerAbstract._expandFOLpredicates(lf)
            if rem_eq:
                self._remove_eq(lf)
            return lf

        out = map(lambda x: to_lf(x, remove_eq, expand_predicates), fol_list)
        return out

    @staticmethod
    def _expandFOLpredicate(fol):
        predicate = fol[0]
        args = fol[1:]
        for pattern, parser in BoxerAbstract._expansion_patterns:
            matching = match(pattern, predicate)
            if matching:
                pred_elems = map(lambda x: [x.lower()], matching.groups())
                out = parser(pred_elems, list(args))
                logger.debug(' {exp} :- {fol}'.format(exp=out, fol=fol))
                return out
        return None

    @staticmethod
    def _expandFOLpredicates(fol, concatenator=FOL.AND):
        if fol is None:
            return None
        frontier = [fol.info]
        while len(frontier) > 0:
            term = frontier.pop()
            predicate = term[0]
            if FOL.is_special(predicate):
                # print 'is special'
                for pos, child in enumerate(term[1:]):
                    expansion = BoxerAbstract._expandFOLpredicate(child)
                    if expansion:
                        if len(expansion) > 1:
                            replacement = [concatenator]  # FOL.AND
                            chain_curr = replacement
                            for child_term in expansion[:-1]:
                                if len(chain_curr) >= 2:
                                    chain_curr.append([concatenator])
                                    chain_curr = chain_curr[-1]
                                chain_curr.append(child_term)
                            chain_curr.append(expansion[-1])
                            term[pos + 1] = replacement
                        else:
                            term[pos + 1] = expansion[0]

                    else:
                        frontier.append(child)
        # print ">>",fol
        return fol

    def sentence2LF(self, sentence, source=None, id=None, expand_predicates=None, **kargs):
        if expand_predicates is None:
            expand_predicates = self.expand_predicates
        if not (source is None or id is None):
            fol = self.sentence2FOL(sentence, source, id)
        else:
            fol = self.sentence2FOL(sentence)
        return self.FOL2LF(fol, expand_predicates, **kargs)

    def _replace_all(self, lf, old_term, new_term):
        frontier = [lf.info]
        while len(frontier):
            curr = frontier.pop()
            pred = curr[0]
            if pred == old_term:
                curr[0] = new_term
            frontier.extend(curr[1:])

    def _remove_eq(self, lf, eq_term='eq'):
        frontier = [lf.info]
        while len(frontier):
            curr = frontier.pop()
            terms = curr[1:]
            pred = curr[0]
            if pred == eq_term:
                old_term = terms[1][0]
                new_term = terms[0][0]
                self._replace_all(lf, old_term, new_term)
            frontier.extend(curr[1:])
        frontier = [lf.info]
        while len(frontier):
            curr = frontier.pop()
            curr[:] = [curr[0]] + [child for child in curr[1:] if child[0] != eq_term]
            frontier.extend(curr[1:])
        pass


class BoxerLocalAPI(Process, BoxerAbstract):
    def __init__(self, tokenizer=None,
                 ccg_parser=None,
                 expand_predicates=True,
                 path_to_bin=config.get('semantic_local', 'boxer'), *params):
        if len(params) == 0:
            params = ('--stdin', '--semantics', 'fol')
        Process.__init__(self, path_to_bin, True, TIME_OUT, *params)

        if tokenizer is None:
            tokenizer = TokenizerLocalAPI()
        if ccg_parser is None:
            ccg_parser = CandCLocalAPI()

        self.name = 'Boxer'
        self.ccg_parser = ccg_parser
        self.tokenizer = tokenizer
        self.expand_predicates = expand_predicates

    def _parsed2FOLstring(self, parsed):
        out, err = self._process(parsed)
        if err:
            # Boxer throws a silly error every time (a bug), we want to ignore it
            if "No source location" not in err:
                print >> stderr, 'Boxer error: {0}'.format(err)
        return out.decode('utf-8').encode("utf-8")

    def _parse_sentence(self, sentence):
        tokenized = self.tokenizer.tokenize(sentence)
        parsed = self.ccg_parser.parse(tokenized)
        return parsed


class BoxerWebAPI(BoxerAbstract):
    def __init__(self, url=config.get('semantic_soap', 'boxer'), expand_predicates=True):
        BoxerAbstract.__init__(self)
        self.url = url
        self.name = 'boxer'
        self.expand_predicates = expand_predicates

    def _parse_sentence(self, sentence):
        return sentence

    def _parsed2FOLstring(self, parsed):
        return post(self.url, data=parsed).text.strip()
