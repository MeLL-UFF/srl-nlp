#!/bin/python
from os           import listdir, path
from sys          import argv,stderr
from fol          import FOL
from boxer        import TokenizerLocalAPI, CandCLocalAPI, BoxerLocalAPI, DependencyTreeLocalAPI
import json
import argparse

class OutAbstract:
    def __init__(self):
        self.docs = []

    def _split_doc(self, doc, sep_section = '\n\n', sep_entity='\n', sep_val=':', **kargs):
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
        '''Deanonimize the text field

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

            doc: document to be analysed
            id: number to identify the document
            analysers: analysers to be used, e.g BoxerLocalAPI, DependencyTreeLocalAPI
            replace: if True, deanonimize the document, default = False

        '''
        return None

    def add_doc(self, doc, id, *analysers, **args):
        self.docs.append(self.process_doc(doc, id, *analysers, **args))

    def dump(self, file):
        pass

    def erase(self):
        self.docs = []

class OutJSON(OutAbstract):
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

class OutPL(OutAbstract):
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
                    out.extend(clause.split())
        return out

    def _lf_to_str(self, lf):
        s = repr(lf)
        if s.startswith('(') or s.startswith(FOL.NOT):
            return '% ' + s
        else:
            return s

    def dump(self, file):
        for doc in self.docs:
            file.write('\n'.join(map(self._lf_to_str, doc)))

def parse_args():
    parser = argparse.ArgumentParser(description = 'Runs the pipeline in a predefined format of documents')
    parser.add_argument('dir_path', help = 'the path to the data files')
    parser.add_argument('out_file', help = 'output file name')
    parser.add_argument('-b', '--break_output', type = int, help='if specified, break the output into BREAK_OUTPUT number of files')
    parser.add_argument('-max','--max_docs', type = int, help = 'maximum number of documents to read')
    parser.add_argument('-out','--output_format', choices=['pl','json'], default= 'json', help = 'format of output')
    parser.add_argument('-e', '--expand_boxer_predicates', action='store_true', help = 'expand Boxer predicates, simplifying its heavy notation')
    parser.add_argument('-q', '--quiet', action='store_true', help = 'supress progress prints')
    parser.add_argument('-rep', '--replace_entities', action='store_true', help = 'replace entity aliases by the originals (deanonimization)')
    args = parser.parse_args()
    return args

def main():
    '''Runs the pipeline in a predefined format of documents
    '''
    args = parse_args()
    #boxer = BoxerWebAPI()
    boxer = BoxerLocalAPI(TokenizerLocalAPI(),
                          CandCLocalAPI(),
                          expand_predicates = args.expand_boxer_predicates)
    depTree    = DependencyTreeLocalAPI()
    base_dir   = args.dir_path
    file_paths = listdir(base_dir)
    if args.max_docs != None: #limit the number of documents read
        length = min(int(args.max_docs), len(file_paths))
    else:
        length = len(file_paths)
    if args.output_format == 'pl': #define the whay the info whil be stored and dumped
        output = OutPL() 
    else:
        output = OutJSON()

    if args.break_output: #create format to generate the output file names
        out_split = argv[2].split('.')
        if len(out_split) > 1:
            prefix    = '.'.join(out_split[:-1])
            extension = out_split[-1]
        else:
            prefix    = out_split
            extension = args.output_format
        out_format = '{0}_%s.{1}'.format(prefix, extension)

    out_count = 0
    #iterate trhough all files in the specified dir
    for count, file_path in enumerate(file_paths[0:length]): #enumerate is used only to set count
        with open(path.join(base_dir, file_path), 'r') as raw_text:
            output.add_doc(raw_text, count, boxer, depTree, replace = args.replace_entities)
        if args.break_output and (count+1) % args.break_output == 0: #save to files
            with open(out_format %out_count, 'w') as out_file:
                output.dump(out_file)
                output.erase()
            out_count += 1
        if not args.quiet: print "%6.2f%%" % (100*float(count+1)/length,) #print progress

    #ensure the last file recives its dump
    if args.break_output:
        if length % args.break_output != 0:
                with open(out_format %out_count, 'w') as out_file:
                    output.dump(out_file)
                if not args.quiet: print "%6.2f%%" % (100*float(count+1)/length,) #print progress
    else:
        with open(args.out_file, 'w') as out_file:
             output.dump(out_file)

if __name__ == '__main__':
    main()
