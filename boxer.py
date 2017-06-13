from requests     import post
from subprocess   import PIPE, Popen
from sys          import stderr
from tempfile     import TemporaryFile
from ConfigParser import ConfigParser
from fol          import FOL, LF
import json, spacy
from regex        import match

config = ConfigParser()
config.read("external.conf")

class Process:
    '''Intended to be an abstract class
    Its subclasses must have an attribute "params" of the type list
    '''
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
    _expansion_patterns = {
        # pattern : lambda variables...: ([predicate, term1,... termN], ...)
        r'^n\d+C64placeholder': lambda  : (['who', []]),
        r'^n\d+numeral':        lambda  : (['numeral',[]]),
        r'^n\d+(.*)':           lambda x: (['noum', [x]],),
        r'^t_X+(\d+)':          lambda x: (['raw_number', [x]],),
        r'^c(\d+)number':       lambda x: (['number', [x]],),
        r'^c\d+numeral':        lambda  : (['numeral',[]]),
        r'^c\d+(.*)':           lambda x: (['cnoum', [x]],),
        r'^a\d+(.*)':           lambda x: (['adjective', [x]],),
        r'^\w\d+(?:A|actor)':   lambda  : (['actor',[]],),
        r'^v\d+C64placeholder': lambda  : (['action',[]],),
        r'^v\d+(.*)':           lambda x: (['verb', [x]],),
        r'^r\d+T|theme':        lambda  : (['theme',[]],),
        r'^r\d+T|topic':        lambda  : (['topic',[]],),
        r'^r\d+(.*)':           lambda x: (['relation', [x]],),
        r'^geonam\d(.*)':       lambda x: (['geoname', [x]],)
    }
    def __init__():
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
        return fols

    def FOL2LF(self, fol_list, expand_predicates, removeForAlls = True, **kargs):
        # print fol_list
        # raw_input()
        to_LF = lambda x: LF(x, removeForAlls=removeForAlls, header = 'fol',**kargs)
        if expand_predicates:
            parse = lambda x: to_LF(BoxerAbstract._expandFOLpredicates(x))
        else:
            parse = to_LF(x)
        out = map(parse, fol_list)
        # print out
        # raw_input()
        return out

    @staticmethod
    def _expandFOLpredicate(fol):
        predicate = fol[0]
        args = fol[1:]
        #print predicate, '-', len(args), '\n\n\n'
        for pattern, parser in BoxerAbstract._expansion_patterns.iteritems():
            matching = match(pattern, predicate)
            if matching:
                out = []
                for p,a in parser(*matching.groups()):
                    if len(a) > 0:
                         out.append([p]+args+[a])
                    else:
                        out.append([p]+args)
                return out
        return None

    @staticmethod
    def _expandFOLpredicates(fol, concatenator = FOL.AND):
        if fol == None:
            return None
        #print '\n\n',fol
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
                print "+"
        return self.FOL2LF(fol, expand_predicates, **kargs)


class BoxerLocalAPI(Process, BoxerAbstract):
    def __init__(self, tokenizer, ccg_parser, expand_predicates, path_to_bin = config.get('semantic_local', 'boxer'), *params):
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
        #print 't:', tokenized
        #print 'c&c:', parsed
        #raw_input()
        return parsed


class BoxerWebAPI(BoxerAbstract):
    def __init__(self, url = config.get('semantic_soap', 'boxer')):
        self.url = url
        self.name = 'boxer'

    def _parse_sentence(self, sentence):
        return sentence

    def _parsed2FOLstring(self, parsed):
        return  post(self.url, data = sentence).text.strip()


class DependencyTreeLocalAPI:
    def __init__(self, model = config.get('syntatic_local', 'spacy_model')):
        self.name = ' '.join(['depTree', model])
        self.parser = spacy.load(model)
        self.count = 0

    def sentence2LF(self, sentence, source = None, id = None, *args, **kargs):
        tree = self.parser(sentence.decode('utf-8'))
        out = []
        ids = dict([(w,'depTree%d' %(self.count+i)) for i, w in enumerate(tree)])
        self.count += len(ids)
        if not (source == None or id == None):
            doc_ref = '%s, %s, ' %(source, id)
        else:
            doc_ref = ''
        for w, i in ids.iteritems():
            out.append('%s(%s%s, \'%s\')' % (w.pos_, doc_ref, i, w.text))
            if w.text.startswith("@"):
                out.append('ENTITY(%s%s)' % (doc_ref, i))
            if w.head != w:
                out.append('%s(%s%s, %s)' % (w.dep_, doc_ref, i, ids[w.head]))
            else:
                out.append('sentence_root(%s%s)' %(doc_ref, i))
        #out = map(lambda x: x.decode('utf-8').encode("utf-8"), out)
        out = (",".join(out)).encode("utf-8")
        return [LF(FOL('%s(%s)' %(FOL.AND, out)))]