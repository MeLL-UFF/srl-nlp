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
from fol import FOL

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


class BoxerAbstract:
    def __init__():
        assert True, 'You should not initialize this class'
    def sentence2FOL(self, sentence):
        parsed    = self._parse_sentence(sentence)
        boxed =  self._parsed2FOLstring(parsed)
        #print boxed
        #return lines that do not start with '%%%', nor 'id' and are not empty
        return filter(lambda x: not (x.startswith('id') or x.startswith('%%%')) and len(x) > 0, boxed.split("\n"))

    def FOL2LF(self, fol_list, expand_predicates):
        # print fol_list
        # raw_input()
        fols = [FOL(fol) for fol in fol_list]
        for fol in fols: fol.skolemize(removeForAlls = False)
        parse = lambda x: BoxerAbstract.expandFOLpredicates(x).str_lf() if expand_predicates else x.str_lf()
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

    def sentence2LF(self, sentence, expand_predicates = None):
        if expand_predicates == None:
            expand_predicates = self.expand_predicates
        return self.FOL2LF(self.sentence2FOL(sentence), expand_predicates)


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

class BoxerWebAPI(BoxerAbstract ):
    def __init__(self, url = config.get('semantic_soap', 'boxer')):
        self.url = url
        self.name = 'boxer'

    def _parse_sentence(self, sentence):
        return sentence

    def _parsed2FOLstring(self, parsed):
        return  post(self.url, data = sentence).text.strip()#, headers=headers)

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
    parser = argparse.ArgumentParser(description = 'Runs the pipeline in a predefined format of documents')
    parser.add_argument('dir_path', help = 'the path to the data files')
    parser.add_argument('out_file', help = 'output file name')
    parser.add_argument('-b', '--break_output', type = int, help='if specified, break the output into BREAK_OUTPUT number of files')
    parser.add_argument('-max','--max_docs', type = int, help = 'maximum number of documents to read')
    parser.add_argument('-e', '--expand_boxer_predicates', action='store_true', help = 'expand Boxer predicates, simplifying its heavy notation')
    args = parser.parse_args()
    return args

if __name__ == '__main__':
    args = parse_args()
    #boxer = BoxerWebAPI()
    boxer = BoxerLocalAPI(TokenizerLocalAPI(), CandCLocalAPI(), expand_predicates = args.expand_boxer_predicates)
    depTree = DependencyTreeLocalAPI()
    doc_list = []
    base_dir = args.dir_path
    file_paths = listdir(base_dir)
    length = min(int(args.max_docs), len(file_paths)) if args.max_docs != None else len(file_paths)

    out_count = 0
    
    if args.break_output:
        out_split = argv[2].split('.')
        out_format = '{0}_%s.{1}'.format(*(('.'.join(out_split[:-1]), out_split[-1]) if len(out_split) > 1 else (out_split, 'txt')))

    for count, file_path in enumerate(file_paths[0:length]): #enumerate is used only to set count
        with open(path.join(base_dir, file_path), 'r') as raw_text:
            info = process_doc(raw_text, boxer, depTree)
            doc_list.append(info)
        print "%6.2f%%" % (100*float(count+1)/length,) #print progress
        if args.break_output and (count+1) % args.break_output == 0: #save to files
            with open(out_format %out_count, 'w') as out:
                json.dump(doc_list, out)
            out_count += 1
            doc_list = []

    if args.break_output:
        if length % args.break_output != 0:
                with open(out_format %out_count, 'w') as out:
                    json.dump(doc_list, out)
    else:
        with open(args.out_file, 'w') as out:
             json.dump(doc_list, out)
