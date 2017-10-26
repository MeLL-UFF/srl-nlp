#!/bin/python
'''
Runs the pipeline in a predefined format of documents
'''
from os                      import listdir, path
from fol                     import FOL
from sys                     import argv,stderr
from random                  import shuffle
from analysers.boxer         import TokenizerLocalAPI, CandCLocalAPI, BoxerLocalAPI
from analysers.depencytree   import DependencyTreeLocalAPI
from logger_config           import config_logger, add_logger_args
import json
import logging
import argparse

logger = logging.getLogger(__name__)

class ProcessorAbstract(object):
    '''
    *Abstract class*
    This class and its children define how to process the input and the output
    for a particular file format.
    '''
    def __init__(self, *args, **kargs):
        '''abstract class initializer'''
        self.docs = []

    def _split_doc(self, doc, sep_section = '\n\n', sep_entity='\n', sep_val=':', **kargs):
        '''Breaks the raw text document into a dictionary.
        {
            'url':url of the news,
            'doc': the text of the news,
            'question': the question from the Cloze Method,
            'answer': the answer,
            'entities_dict': a dictionary maping the aliases to the real entities
        }
        '''
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

    def replace_entities(self, doc, text):
        '''
        Deanonimizes the text field

        Params:
            doc: document object with information on how to replace entities
            text: text to go through the replacements

        This method returns the original text with all aliases (@entityXX)replace by its original tokens.
        '''
        entities_map = doc['entities_dict']
        entities = sorted(entities_map.keys(), lambda x,y: len(y) - len(x)) #sort by length to avoid replacing '@entity1' in '@entity12', for instance
        for entity in entities:
            text = text.replace(entity, entities_map[entity])
        return text

    def process_doc(self, doc, id, *analysers, **args):
        '''Returns a structured document with the analysers results
            Params:
                doc: document to be analysed
                id: number to identify the document
                analysers: analysers to be used, e.g BoxerLocalAPI, DependencyTreeLocalAPI
                replace: if True, deanonimize the document, default = False
        '''
        return None

    def add_doc(self, doc, id, *analysers, **args):
        '''Adds this doc to the list of docs to be dumped in a file later
        '''
        self.docs.append(self.process_doc(doc, id, *analysers, **args))

    def dump(self, file):
        '''Dumps the documents to file (a stream)'''
        assert True, 'abstract method'

    def erase(self):
        '''Clears the data stored'''
        self.docs = []

    def _get_name_formatter(self, out_file, default_extension, break_output = False, force_extension = False):
        '''Build a formatter to handle the output file names

        _get_name_formatter(...) -> f(extension, id) -> str

        Params:
            out_file_name: the desired output file name
            default_extension: the default extension for this kind of output
            breaks_output: a boolean value
            force_extension: a boolean value to force the default_extension

        Returns:
            Returns a function that returns a string.
            The function 'format takes as arguments extension and id (optional)

            Params of f:
                extension: the desired extension (might be negleted)
                id: the desired id (might be negleted)
        '''
        prefix, extension = path.splitext(out_file)
        if len(extension) == 0:
                extension = default_extension[1:]
        if break_output:
            if force_extension:
                out_str = '%s_{1}.{0}' %prefix
            else:
                out_str = '%s_{1}.%s' % (prefix, extension)
        else:
            if force_extension:
                out_str = '%s.{0}' %prefix
            else:
                out_str = '%s.%s' %(prefix, extension)
        return out_str.format

    def save_output(self, file_name, id = None):
        '''
        Parsers the file_name and generate the output file_name based on it
        '''
        formatter = self._get_name_formatter(file_name, self.default_extension, id != None)
        out_name = formatter(default_extension,id)
        logger.info('Writing to file %s', out_name)
        with open(out_name,'w') as f:
            self.dump(f)


class Processor2JSON(ProcessorAbstract):
    '''
    This class defines how to process the input and the output files for a JSON
    output.
    '''

    def __init__(self,*args, **kargs):
        super(Processor2PL, self).__init__(args, kargs)
        self.default_extension = 'json'
    def process_doc(self, doc, id, *analysers, **args):
        info = self._split_doc(doc, **args)
        # given a doc, generate a dict {analyser_i: analyser_i.sentence2FOL(doc)}
        join        = lambda x: '\n'.join(map(repr, x))
        analyse_doc = lambda x,src,id: (x.name, join(x.sentence2LF(doc, src,id)))
        analyse     = lambda doc,src,id: dict(map(lambda a: analyse_doc(a,src,id), analysers))
        for text_field in ['doc', 'question']:
            if args.get('replace', False):
                text = self.replace_entities(info, info[text_field])
            else:
                text = info[text_field]
            info[text_field] = analyse(text, text_field[0], id)
            info[text_field]['text'] = text
        return info

    def dump(self, file):
        json.dump(self.docs, file)


class Processor2PL(ProcessorAbstract):
    '''
    This class defines how to process the input and the output files for a Prolog
    output.
    '''
    def __init__(self, store_f = True, store_n = True, *args, **kargs):
        super(Processor2PL, self).__init__(args, kargs)
        self.facts     = []
        self.negatives = []
        self.store_f   = store_f
        self.store_n   = store_n
        self.default_extension = 'pl'

    def process_doc(self, doc, id,*analysers, **args):
        info = self._split_doc(doc, **args)
        analyse = lambda doc,src,id: map(lambda a: a.sentence2LF(doc,src,id), analysers)
        out = []
        for text_field in ['doc', 'question']:
            if args.get('replace', False):
                text = self.replace_entities(info, info[text_field])
            else:
                text = info[text_field]
            for result in analyse(text, text_field[0], id):
                for clause in result:
                    out.extend(self._str_base(*clause.split()))
        if self.store_f:
            self.facts.append(self._str_fact('answer',id, info['answer']))
        if self.store_n:
            for entity in info['entities_dict']:
                if entity != info['answer']:
                    self.negatives.append(self._str_neg('answer', id, entity))
        return out

    def _str_base(self, *args):
        ignored = lambda s: s.startswith('(') or s.startswith(FOL.NOT)
        comment_filter = lambda x: ('% ' + x) if ignored(x) else x
        handle_lf = lambda lf: comment_filter(str(lf))
        return map(handle_lf, args)

    def _str_fact(self, pred, *args):
        return "%s(%s).\n" %(pred, ','.join(map(str, args)))

    def _str_neg(self,pred, *args):
        return self._str_fact(pred, *args)

    def dump(self, file):
        for doc in self.docs:
            file.write('\n'.join(doc))
            file.write('\n')

    def dump_neg(self, file):
        for doc in self.negatives:
            file.write(doc)

    def dump_fact(self, file):
        for doc in self.facts:
            file.write(doc)

    def save_output(self, file_name, id = None):
        '''
        Parsers the file_name and generate the outputs file_name based on it.
        After that it dumps the contents to the outputs file_name
        '''
        formatter = self._get_name_formatter(file_name, self.default_extension, id != None, True)
        logger.info('Writing to files %s', formatter("*",id))
        for ext, dump in [('n',  self.dump_neg),
                          ('pl', self.dump),
                          ('f',  self.dump_fact)]:
            with open(formatter(ext, id),'w') as f:
                dump(f)

    def erase(self):
        '''Clears the data stored'''
        super(Processor2PL, self).erase()
        self.facts     = []
        self.negatives = []


class Processor2ProbLog(Processor2PL):
    '''
    This class defines how to process the input and the output files for a Prolog
    output.
    '''
    def _str_base(self, *args):
        ignored = lambda s: s.startswith('(') or s.startswith(FOL.NOT)
        comment_filter = lambda x: ('% ' if ignored(x) else '') + '0.5:: ' + x
        handle_lf = lambda lf: comment_filter(str(lf))
        return map(handle_lf, args)

    def _str_fact(self, pred, *args):
        return "1.0:: %s(%s).\n" %(pred, ','.join(map(str, args)))

    def _str_neg(self, pred, *args):
        return "0.0:: %s(%s).\n" %(pred, ','.join(map(str, args)))


def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Runs the pipeline in a predefined format of documents')
    parser.add_argument('dir_path', help = 'the path of the data files')
    parser.add_argument('out_file', help = 'output file name')
    parser.add_argument('-b', '--break_output', type = int,
                        help='if specified, break the output into BREAK_OUTPUT number of files')
    parser.add_argument('-max','--max_docs', type = int,
                        help = 'maximum number of documents to read')
    group = parser.add_mutually_exclusive_group()
    group.add_argument ('-skip', type = int, default= 0,
                        help = 'skips first SKIP questions')
    group.add_argument ('-random', action='store_true',
                        help = 'randomly choose the questions')
    parser.add_argument('-out','--output_format',choices=['pl','problog','json'],
                        default= 'json', help = 'format of output')
    parser.add_argument('-e', '--expand_boxer_predicates', action='store_true',
                        help = 'expand Boxer predicates, simplifying its heavy notation')
    #parser.add_argument('-q', '--quiet', action='store_true', help = 'supress progress prints')
    parser.add_argument('-rep', '--replace_entities', action='store_true',
                        help = 'replace entity aliases by the originals (deanonimization)')
    add_logger_args(parser)
    args = parser.parse_args()
    return args


def main(argv):
    '''
    Runs the pipeline in a predefined format of documents
    '''
    args   = parse_args(argv, add_logger_args)
    config_logger(args)

    logger.info('Initializing Boxer')
    #boxer = BoxerWebAPI()
    boxer = BoxerLocalAPI(TokenizerLocalAPI(),
                          CandCLocalAPI(),
                          expand_predicates = args.expand_boxer_predicates)
    logger.info('Initializing Dependence Tree')
    depTree    = DependencyTreeLocalAPI()
    base_dir   = args.dir_path
    file_paths = listdir(base_dir)

    if args.random:
        shuffle(file_paths)

    if args.max_docs != None: #limit the number of documents read
        length = min(int(args.max_docs), len(file_paths))
    else:
        length = len(file_paths)

    if args.output_format == 'pl': #define the whay the info whil be stored and dumped
        output = Processor2PL()
    elif args.output_format == 'problog':
        output = Processor2ProbLog()
    else:
        output = Processor2JSON()

    out_count = 0
    #iterate trhough all files in the specified dir
    #args.out_file
    logger.info('Reading files')
    for count, file_path in enumerate(file_paths[args.skip:(args.skip+length)]): #enumerate is used only to set count
        with open(path.join(base_dir, file_path), 'r') as raw_text:
            output.add_doc(raw_text, count, boxer, depTree, replace = args.replace_entities)
        if args.break_output and (count+1) % args.break_output == 0: #save to files
            output.save_output(args.out_file, out_count)
            output.erase()
            out_count += 1
        logger.info("%6.2f%% Read", 100*float(count+1)/length) #logs progress

    #ensure the last file recives its dump
    if args.break_output:
        if length % args.break_output != 0:
                output.save_output(args.out_file, out_count)#logs progress
    else:
        output.save_output(args.out_file)

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by user')
