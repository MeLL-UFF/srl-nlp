from requests import post
from subprocess import PIPE, Popen
from os import listdir, path
from sys import argv,stderr
from tempfile import TemporaryFile
import json
import spacy
import regex
import argparse
from ConfigParser import ConfigParser

config = ConfigParser()
config.read("external.conf")

class Process:
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

class FOL:
    NOT    = 'not'
    EXISTS = 'some'
    AND    = 'and'
    OR     = 'or'
    ALL    = 'all'
    def __init__(self, text):
        self.info = FOL.parse(text.strip())

    @staticmethod
    def parse(text):
        return FOL._parse_aux(FOL._split(text)) if len(text) > 0 else []

    @staticmethod
    def _parse_aux(queue):
        aux = []
        balance = 0
        predicate = []
        if len(queue) == 0:
            return None
        while len(queue) > 0:
            term = queue.pop(0)
            if term == '(':
                predicate.append(FOL._parse_aux(queue))
                balance += 1
            elif term == ')':
                if len(predicate) < 2:
                    queue.insert(0, term)
                else:
                    balance -= 1
                break
            elif term == ',':
                if len(predicate) < 2:
                    queue.insert(0, term)
                    break
                predicate.append(FOL._parse_aux(queue))
            else:
                predicate.insert(0, term.strip().replace("'", "\\'").replace('"', '\\"'))
        assert balance == 0, 'Error parsing FOL'
        return predicate


    @staticmethod
    def _split(text, sep = ',()', include_sep = True, filter = ['', '.']):
        queue = []
        token = ''
        for l in text:
            if l in sep:
                if len(token) > 0: queue.append(token.strip())
                token = ''
                if include_sep: queue.append(l)
            else:
                token += l
        if len(token) > 0: queue.append(token.strip())
        if queue[-1] == '.': queue.pop()
        return queue


    def skolemize(self, has_header = True, removeForAlls = False, ignore=['@placeholder']):
        '''This method converts the FOL to its Skolem form.
        has_header: defines if the first predicate should be ignored
        removesForAlls: tells if we should eliminate the universal quantifiers too
        ignore: provides a list of constants or varibles that should be left unchanged (even if their quantifiers are )
        '''
        self.convert2PrenexForm()
        constants = []
        term = self.info[2] if has_header else self.info
        #Find all existential variables
        oldTerm = None
        while term[0] == FOL.EXISTS or term[0] == FOL.ALL:
            if removeForAlls or not term[0] == FOL.ALL:
                if term[1][0] not in ignore:
                    constants.extend(term[1])
                if oldTerm:
                    oldTerm[2] = term[2]
                else:
                    oldTerm = term
                    self.info = term
            term = term[2]

        frontier = [term]
        #search to replace every existential variable by a constant
        while len(frontier) > 0:
            child = frontier.pop()
            if len(child) < 2:
                if child[0]  in constants:
                    child[0] = 'c%s' %constants.index(child[0]) #TODO universal constants
            else:
                frontier.extend(child[1:])


    def convert2PrenexForm(self, header = 'fol'):
        if self.info[0] == header:
            term = self.info[2]
        else:
            term = self.info
        if len(term) < 2:
            return term
        term = FOL._pushNegation(term)
        term = FOL._pushQuantifiers(term)
        if self.info[0] == header:
            self.info[2] = term
        else:
            self.info = term

    @staticmethod
    def _pushQuantifiers(term):
        if len(term) < 2:
            return term
        if term[0] != FOL.ALL and term[0] != FOL.EXISTS:
            for pos, child in enumerate(term[1:]):
                if child[0] == FOL.ALL or child[0] == FOL.EXISTS:
                    quantTail = child[2]
                    child[2] = term
                    term[1+pos] = quantTail
                    term = child
                    break
        for pos, child in enumerate(term[1:]):
            term[1+pos] = FOL._pushQuantifiers(child)
        return term

    #tries to strip all quantifiers and operations from negation
    @staticmethod
    def _pushNegation(term):
        if len(term) < 2:
            return term
        if term[0] == FOL.NOT:
            term = FOL._negate(term[1])
        for pos, child in enumerate(term[1:]):
            term[1+pos] = FOL._pushNegation(child)
        return term


    @staticmethod
    def _negate(term):
        if term[0] == FOL.NOT:
            return term[1] # not(not(x)) = x
        if term[0] == FOL.AND:
            return [FOL.OR]+ map(FOL._negate, term[1:])  # not(and(x,y)) = or(not(x), not(y))
        if term[0] == FOL.OR:
            return [FOL.AND]+ map(FOL._negate, term[1:]) # not(or(x,y)) = and(not(x), not(y))
        if term[0] == FOL.ALL:
            return [FOL.EXISTS, term[1]]+ map(FOL._negate, term[2:]) # not(all(X,y)) = exists(X, not(y))
        if term[0] == FOL.EXISTS:
            return [FOL.ALL, term[1]] + map(FOL._negate, term[2:])    # not(exists(X,y)) = all(X, not(y))
        return [FOL.NOT, term]


    @staticmethod
    def toLF(fol, available_ids = None):
        return ""

    def _has_left_child(self):
        return self.info != None and len(self.info) > 1 and self.info[1] != None

    def _has_right_child(self):
        return self.info != None and len(self.info) > 2 and self.info[2] != None

    #TODO: it supposes that there are at most 2 variables in a term
    def _has_children(self):
        return self._has_left_child(self) or self._has_right_child(self)

    @staticmethod
    def _str_aux(info):
        if info == None:
            out = ''
        else:
            out = info[0]
            if self._has_children():
                out += '(%s)' % ','.join(map(self._str_aux, info[1:]))
        return out

    @staticmethod
    def _str_lf(info, and_t = ',', or_t = ';', header = 'fol'):
        if info == None:
            out = ''
        else:
            if info[0] == FOL.AND:
                out = '%s' % and_t.join(map(FOL._str_lf, info[1:]))
            elif info[0] == FOL.OR:
                out = '%s' % or_t.join(map(FOL._str_lf, info[1:]))
            elif info[0] == FOL.ALL or info[0] == FOL.EXISTS:
                out = FOL._str_lf(info[2])
            else:
                out = info[0]
                if len(info) > 1:
                    out += '(%s)' % ','.join(map(FOL._str_lf, info[1:]))
        return out

    def str_lf(self, and_t = ',', or_t = ';', header = 'fol'):
        info = self.info[2] if self.info == header else self.info
        return FOL._str_lf(info, and_t, or_t, header)+ '.'

    def __repr__(self):
        return FOL._str_aux(self.info) + '.'

class TokenizerLocalAPI(Process):
    def __init__(self, path_to_bin = config.get('semantic_local', 't'), *params):
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

class BoxerLocalAPI(Process):
    def __init__(self, tokenizer, ccg_parser, path_to_bin = config.get('semantic_local', 'boxer'),*params):
        if len(params) == 0:
            params = ('--stdin', '--semantics', 'fol')
        self.name        = 'Boxer'
        self.path_to_bin = path_to_bin
        self.ccg_parser  = ccg_parser
        self.tokenizer   = tokenizer
        self.params      = params

    def parsed2FOL(self, parsed):
        out, err = self._process(parsed)
        if err:
            # Boxer throws a silly error every time (a bug), we want to ignore it
            if not "No source location" in err:
                print >> stderr, 'Boxer error: {0}'.format(err)
        return out.decode('utf-8').encode("utf-8")

    def sentence2FOL(self, sentence):
        tokenized = self.tokenizer.tokenize(sentence)
        parsed    = self.ccg_parser.parse(tokenized)
        #print 't:', tokenized
        #print 'c&c:', parsed
        #raw_input()
        boxed =  self.parsed2FOL(parsed)
        #print boxed
        #return lines that do not start with '%%%', nor 'id' and are not empty
        return filter(lambda x: not (x.startswith('id') or x.startswith('%%%')) and len(x) > 0, boxed.split("\n"))

    #TODO
    def FOL2LF(self, fol_list, expand_predicates = True):
        # print fol_list
        # raw_input()
        fols = [FOL(fol) for fol in fol_list]
        for fol in fols: fol.skolemize(removeForAlls = False)
        parse = lambda x: BoxerLocalAPI.expandFOLpredicates(x).str_lf() if expand_predicates else x.str_lf()
        out = '\n'.join(map(parse, fols))
        # print out
        # raw_input()
        return out

    @staticmethod
    def expandFOLpredicates(fol):
        # frontier = [fol]
        # while len(frontier) > 0:
        #     if fol[0] == FOL.AND or fol[0] == FOL.OR:
        #         children = fol[2:]
        #         for pos, child in enumerate(children)
        #             expanded = self.expand_FOLpredicate(child[0])
        #             if len(expanded)> 1:
        #                 pass #TODO
        return fol

    def sentence2LF(self, sentence):
        return self.FOL2LF(self.sentence2FOL(sentence))

class BoxerWebAPI:
    def __init__(self, url = config.get('semantic_soap', 'boxer')):
        self.url = url
        self.name = 'boxer'

    def sentence2FOL(self, sentence):
        out =  post(self.url, data = sentence).text.strip()#, headers=headers)
        #return lines that do not start with '%%%'
        return filter(lambda x: not x.startswith('%%%'), out.split("\n"))


class DependencyTreeLocalAPI:
    def __init__(self, model = config.get('syntatic_local', 'spacy_model')):
        self.name = ' '.join(['depTree', model])
        self.parser = spacy.load(model)
        self.count = 0

    def sentence2LF(self, sentence):
        tree = self.parser(sentence.decode('utf-8'))
        out = []
        ids = dict([(w,'depTree%d' %(count+i)) for i, w in enumerate(tree)])
        self.count += len(tree)
        for w, i in ids.iteritems():
            out.append('%s(%s, \'%s\')' % (w.pos_, i, w.text))
            if w.text.startswith("@"):
                out.append('ENTITY(%s)' % i)
            if w.head != w:
                out.append('%s(%s, %s)' % (w.dep_, i, ids[w.head]))
            else:
                out.append('sentence_root(%s)' %i)
        return ", ".join(out)


def _split_doc(doc, sep_section = '\n\n', sep_entity='\n', sep_val=':'):
    if hasattr(doc, 'read'):
        doc = doc.read()
    info = doc.split(sep_section)
    if len(info) < 5:
        raise Exception('Error parsing document')
    info[4] = dict(map(lambda x: x.split(sep_val,1), info[4].split(sep_entity)))
    return {
            'url':info[0],
            'doc':info[1],
            'question':info[2],
            'answer':info[3],
            'entities_dict':info[4],
           }

def process_doc(doc, *analysers, **args):
    info = _split_doc(doc, **args)
    # given a doc, generate a dict {analyser_i: analyser_i.sentence2FOL(doc)}
    analyse = lambda doc: dict(map(lambda a: (a.name,a.sentence2LF(doc)), analysers)) 
    for text_field in ['doc', 'question']:
        text = info[text_field]
        info[text_field] = analyse(info[text_field])
        info[text_field]['text'] = text
    return info


def parse_args():
    parser = argparse.ArgumentParser(description = 'Runs the pipeline of boxer in a predefined format of documents')
    parser.add_argument('dir_path', help = 'the path to the data files')
    parser.add_argument('out_file', help = 'output file name')
    parser.add_argument('-b', '--break_output', type = int, help='if specified, break the output into BREAK_OUTPUT number of files')
    parser.add_argument('-max','--max_docs', type = int, help = 'maximum number of documents to read')
    parser.add_argument('-e', '--expand_boxer_predicates',action='store_true', help = 'expand Boxer predicates, simplifying its heavy notation')
    args = parser.parse_args()
    return parser

if __name__ == '__main__':
    args = parse_args()
    #boxer = BoxerWebAPI()
    boxer = BoxerLocalAPI(TokenizerLocalAPI(), CandCLocalAPI())
    depTree = DependencyTreeLocalAPI()
    doc_list = []
    base_dir = args.dir_path
    file_paths = listdir(base_dir)
    length = min(int(argv[3]), len(file_paths)) if args.max_docs else len(file_paths)

    out_count = 0
    
    if args.break_output:
        out_split = argv[2].split('.')
        out_format = '%s_{0}.%s' %(('.'.join(out_split[:-1]), out_split[-1]) if len(out_split) > 1 else (out_split, 'txt'))

    for count, file_path in enumerate(file_paths[0:length]): #enumerate is used only to set count
        with open(path.join(base_dir, file_path), 'r') as raw_text:
            info = process_doc(raw_text, boxer, depTree)
            doc_list.append(info)
        print "%6.2f%%" % (100*float(count+1)/length,) #print progress
        if args._break_output and (count+1) % args._break_output == 0: #save to files
            with open(out_format %out_count, 'w') as out:
                json.dump(doc_list, out)
            out_count += 1
            doc_list = []

    if args._break_output:
        if length % args._break_output != 0:
                with open(out_format %out_count, 'w') as out:
                    json.dump(doc_list, out)
    else:
        with open(args.out_file, 'w') as out:
             json.dump(doc_list, out)
