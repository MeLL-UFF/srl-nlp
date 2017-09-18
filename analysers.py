from requests               import post
from subprocess             import PIPE, Popen
from sys                    import stderr
from os                     import path
from tempfile               import TemporaryFile
from ConfigParser           import ConfigParser
from fol                    import FOL
from logicalform            import LF
from regex                  import match, compile
from stanford_parser.parser import Parser as StanfordParser
import json, spacy
import logging

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))

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
        r'^n\d+c64placeholder': lambda  : (['who', []]),
        r'^n\d+numeral':        lambda  : (['numeral',[]]),
        r'^n\d+(.*)':           lambda x: (['noum', [x]],),
        r'^t_X+(\d+)':          lambda x: (['raw_number', [x]],),
        r'^c(\d+)number':       lambda x: (['number', [x]],),
        r'^c\d+numeral':        lambda  : (['numeral',[]]),
        r'^c\d+(.*)':           lambda x: (['cnoum', [x]],),
        r'^a\d+(.*)':           lambda x: (['adjective', [x]],),
        r'^\w\d+(?:A|actor)':   lambda  : (['actor',[]],),
        r'^v\d+c64placeholder': lambda  : (['action',[]],),
        r'^v\d+(.*)':           lambda x: (['verb', [x]],),
        r'^r\d+T|theme':        lambda  : (['theme',[]],),
        r'^r\d+T|topic':        lambda  : (['topic',[]],),
        r'^r\d+(.*)':           lambda x: (['relation', [x]],),
        r'^geonam\d(.*)':       lambda x: (['geoname', [x]],)
    }

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
    def __init__(self, tokenizer = TokenizerLocalAPI(), ccg_parser = CandCLocalAPI(), expand_predicates = True, path_to_bin = config.get('semantic_local', 'boxer'), *params):
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
        out  = []
        ids  = dict([(w,'depTree%d' %(self.count+i)) for i, w in enumerate(tree)])
        format_text = lambda x: x.replace("'", "\\'").lower()
        self.count += len(ids)
        if not (source == None or id == None):
            doc_ref = '%s, %s, ' %(source, id)
        else:
            doc_ref = ''
        for w, i in ids.iteritems():
            out.append('%s(%s%s, \'%s\')' % (format_text(w.pos_), doc_ref, i, format_text(w.text)))
            if w.text.startswith("@"):
                out.append('entity(%s%s)' % (doc_ref, i))
            if w.head != w:
                out.append('%s(%s%s, %s)' % (w.dep_, doc_ref, i, ids[w.head]))
            else:
                out.append('sentence_root(%s%s)' %(doc_ref, i))
        #out = map(lambda x: x.decode('utf-8').encode("utf-8"), out)
        out = (",".join(out)).encode("utf-8")
        text = '%s(%s).' %(FOL.AND, out)
        fol = FOL(text)
        #print '\n*:', text
        #print '\n>',fol
        return [LF(fol)]


class DependencyTreeBeltagyLocalAPI:
    _drop_tags  = ('det', 'det:predet', 'neg', 'case', 'dislocated', 'remnant', 'discourse',
                   'aux', 'auxpass', 'cop', 'mark', 'punct', 'cc', 'cc:preconj')
    _merge_tags = ('nmod:npmod', 'amod', 'advmod', 'compound', 'compound:prt', 'name', 'mwe',
                   'foreign', 'goeswith', 'prt')
    _new_tags   = ('nsubj', 'nsubjpass', 'dobj', 'iobj', 'csubj', 'csubjpass', 'ccomp',
                   'xcomp', 'nummod', 'appos', 'nmod', 'nmod:poss', 'nmod:tmod', 'acl',
                   'acl:relcl', 'advcl', 'list', 'parataxis', 'reparandum', 'vocative',
                   'expl', 'conj', 'dep', 'npadvmod')

    class _Dependence:
        def __init__(self, rel, gov, dep):
            self.dep = dep
            self.rel = rel
            self.gov = gov
            self.children = []

        def __str__(self):
            return '%s -%s-> %s' %(self.dep.text, self.rel, self.gov.text)
        def __repr__(self):
            return self__str__()
        def __hash__(self):
            return (self.dep, self.rel, self.gov).__hash__()


    def __init__(self, *args, **kargs):
        self._init_parser(*args, **kargs)
        self.count = 0
        self.rel_classifier = dict()

        for i in self._drop_tags:
            self.rel_classifier[i] = self._drop
        for i in self._merge_tags:
            self.rel_classifier[i] = self._merge
        for i in self._new_tags:
            self.rel_classifier[i] = self._new
        self.rel_classifier['ROOT'] = self._root

    def _init_parser(self, *args, **kargs):
        self.name = 'depTreeUD'
        self.parser = StanfordParser()

    def _parse_sentence_tree(self, sentence):
        parsing = self.parser.parseToStanfordDependencies(sentence.decode('utf-8'))
        dependencies = parsing.dependencies
        tree = dict()

        for rel,gov,dep in dependencies:
            struct_dep = tree.get(dep, None)
            if struct_dep == None:
                struct_dep = self._Dependence(rel,gov,dep)
                tree[dep] = struct_dep
            else: #if there is a entry in the dict, update it or it might be seen as root
                struct_dep.gov = gov
                struct_dep.rel = rel
            parent = tree.get(gov, self._Dependence('ROOT',gov,gov)) #if there is not a gov representation, create it
            parent.children.append(struct_dep)
            tree[gov] = parent

        #for i in tree:
        #    print tree[i]

        for dep in tree:
            if tree[dep].rel == 'ROOT':
                return tree[dep]
        return None


    def _node2text(self,node):
        return node.text


    def _new_variable(self):
        self.count += 1
        return 'depTreeB%d' % self.count


    def _drop(self, dep, rel, gov, variables_map):
        return []


    def _merge(self, dep, rel, gov, variables_map):
        #print variables_map
        u = variables_map[gov]
        variables_map[dep] = u
        return ['%s(%s)' %(self._node2text(dep), u)]


    def _new(self, dep, rel, gov, variables_map):
        u = variables_map[gov]
        v = self._new_variable()
        variables_map[dep] = v
        return ['%s(%s)' %(self._node2text(dep), u),
                '%s(%s,%s)' %(rel, u, v)]


    def _root(self, dep, rel, gov, variables_map):
        u = self._new_variable()
        variables_map[dep] = u
        return ['sentence_root(%s)' %u,
                '%s(%s)' %(self._node2text(gov), u)]


    def sentence2LF(self, sentence, source = None, id = None, *args, **kargs):
        root = self._parse_sentence_tree(sentence)
        out  = list()
        variables_map = dict()
        frontier = [root]

        while(len(frontier) > 0):
            child = frontier.pop()
            try:
                decision = self.rel_classifier[child.rel]
            except KeyError:
                raise Exception("'%s' is not a recognized tag" %child.dep_)
            child_parsed = decision(child.dep,
                                    child.rel,
                                    child.gov,
                                    variables_map)
            out.extend(child_parsed)
            frontier.extend(child.children)
        out = (",".join(out)).encode("utf-8")
        text = '%s(%s).' %(FOL.AND, out)
        fol = FOL(text)
        return [LF(fol)]


class DependencyTreeBeltagySpaCyLocalAPI(DependencyTreeBeltagySpaCyLocalAPI):
    #TODO add specific tag lists
    def _init_parser(self, model = config.get('syntatic_local', 'spacy_model'), *args, **kargs):
        self.name = 'depTreeBeltagySpacy'
        self.parser = spacy.load(model)

    def _parse_sentence_tree(self, sentence):
        tree = self.parser(sentence.decode('utf-8'))
        parsing = self.parser.parseToStanfordDependencies(sentence.decode('utf-8'))
        dependencies = self.parser(sentence.decode('utf-8'))
        tree = dict()

        for node in dependencies:
            gov = node.head
            rel = node.dep_
            dep = node
            struct_dep = tree.get(dep, None)
            if struct_dep == None:
                struct_dep = self._Dependence(rel,gov,dep)
                tree[dep] = struct_dep
            else: #if there is an entry in the dict, update it or it might be seen as root
                struct_dep.gov = gov
                struct_dep.rel = rel
            parent = tree.get(gov, self._Dependence('ROOT',gov,gov)) #if there is not a gov representation, create it
            parent.children.append(struct_dep)
            tree[gov] = parent

        for dep in tree:
            if tree[dep].rel == 'ROOT':
                return tree[dep]
        return None


    def _node2text(self,node):
        return str(node)