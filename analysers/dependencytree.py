
from os                             import path
from ConfigParser                   import ConfigParser
from srl_nlp.fol                    import FOL
from srl_nlp.logicalform            import LF
from srl_nlp.stanford_parser.parser import Parser as StanfordParser
import spacy
import logging

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "../external.conf"))


class DependencyTreeLocalAPI:
    def __init__(self, model = config.get('syntatic_local', 'spacy_model')):
        self.name = ' '.join(['depTree', model])
        self.parser = spacy.load(model)
        self.count = 0

    def sentence2LF(self, sentence, source = None, id = None, *args, **kargs):
        '''Translates an english sentence into its LF representation
        str -> LF
        '''
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
                   'foreign', 'goeswith', 'prt', 'nn','prep_at')
    _new_tags   = ('nsubj', 'nsubjpass', 'dobj', 'iobj', 'csubj', 'csubjpass', 'ccomp',
                   'xcomp', 'nummod', 'appos', 'nmod', 'nmod:poss', 'nmod:tmod', 'acl',
                   'acl:relcl', 'advcl', 'list', 'parataxis', 'reparandum', 'vocative',
                   'expl', 'conj', 'dep', 'npadvmod', 'conj_and','agent')

    class _Dependence:
        '''
        Internal class for storing and processing the dependence links between tokens
        '''
        def __init__(self, rel, gov, dep):
            self.dep = dep
            self.rel = rel
            self.gov = gov
            self.children = []

        def __str__(self):
            return '%s -%s-> %s' %(self.dep.text, self.rel, self.gov.text)
        def __repr__(self):
            return self.__str__()
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
        '''
        Generates the dependence tree for the sentence
        '''
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
            print struct_dep

        #for i in tree:
        #    print tree[i]

        for dep in tree:
            if tree[dep].rel == 'ROOT':
                return tree[dep]
        return None


    def _node2text(self,node):
        '''
        Process the dependence tree node

        _Dependence -> str
        '''
        return node.text


    def _new_variable(self):
        self.count += 1
        return 'depTreeB%d' % self.count


    def _drop(self, dep, rel, gov, variables_map):
        '''
        Return the predicates generated by a Drop relation
        (it is always an empty list)
        '''
        return []


    def _merge(self, dep, rel, gov, variables_map):
        '''
        Return the predicates generated by a Merge relation
        '''
        #print variables_map
        u = variables_map[gov]
        variables_map[dep] = u
        return ['%s(%s)' %(self._node2text(dep), u)]


    def _new(self, dep, rel, gov, variables_map):
        '''
        Return the predicates generated by a New relation
        '''
        u = variables_map[gov]
        v = self._new_variable()
        variables_map[dep] = v
        return ['%s(%s)' %(self._node2text(dep), u),
                '%s(%s,%s)' %(rel, u, v)]


    def _root(self, dep, rel, gov, variables_map):
        '''
        Return the predicates generated by a ROOT relation
        '''
        u = self._new_variable()
        variables_map[dep] = u
        return ['sentence_root(%s)' %u,
                '%s(%s)' %(self._node2text(gov), u)]


    def sentence2LF(self, sentence, source = None, id = None, *args, **kargs):
        '''Translates an english sentence into its LF representation
        str -> LF
        '''
        root = self._parse_sentence_tree(sentence)
        out  = list()
        variables_map = dict()
        frontier = [root]

        while(len(frontier) > 0):
            child = frontier.pop()
            try:
                decision = self.rel_classifier[child.rel]
            except KeyError:
                raise Exception("'%s' is not a recognized tag" %child.rel)
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


class DependencyTreeBeltagySpaCyLocalAPI(DependencyTreeBeltagyLocalAPI):
    #TODO add specific tag lists
    def _init_parser(self, model = config.get('syntatic_local', 'spacy_model'), *args, **kargs):
        self.name = 'depTreeBeltagySpacy'
        self.parser = spacy.load(model)


    def _parse_sentence_tree(self, sentence):
        dependencies = self.parser(sentence.decode('utf-8'))
        tree = dict()

        for node in dependencies:
            gov = node.head
            rel = node.dep_
            dep = node
            struct_dep = tree.get(dep, None)
            if struct_dep == None:
                struct_dep = self._Dependence(rel,gov,dep)
                tree[dep]  = struct_dep
            else: #if there is an entry in the dict, update it or it might be seen as root
                struct_dep.gov = gov
                struct_dep.rel = rel
            parent  = tree.get(gov, self._Dependence('ROOT',gov,gov)) #if there is not a gov representation, create it
            if gov != dep:
                parent.children.append(struct_dep)
            tree[gov] = parent

        for dep in tree:
            if tree[dep].rel == 'ROOT':
                return tree[dep]
        return None


    def _node2text(self,node):
        return str(node)