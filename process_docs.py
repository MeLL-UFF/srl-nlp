#!/bin/python
from os           import listdir, path
from sys          import argv,stderr
from boxer        import TokenizerLocalAPI, CandCLocalAPI, BoxerLocalAPI, DependencyTreeLocalAPI
import json
import argparse

def _split_doc(doc, sep_section = '\n\n', sep_entity='\n', sep_val=':', **kargs):
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

def replace_entities(doc, text):
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

def process_doc(doc, *analysers, **args):
    '''Returns a structured document with the analysersresults

        param doc: document to be analysed
        analysers: analysers to be used, e.g BoxerLocalAPI, DependencyTreeLocalAPI
        replace: if True, deanonimize the document, default = False

    '''
    info = _split_doc(doc, **args)
    # given a doc, generate a dict {analyser_i: analyser_i.sentence2FOL(doc)}
    join = lambda x: '\n'.join(map(repr, x))
    analyse = lambda doc: dict(map(lambda a: (a.name,join(a.sentence2LF(doc))), analysers))
    for text_field in ['doc', 'question']:
        if args.get('replace', False):
            text = replace_entities(info, info[text_field])
        else:
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
    parser.add_argument('-q', '--quiet', action='store_true', help = 'supress progress prints')
    parser.add_argument('-rep', '--replace_entities', action='store_true', help = 'replace entity aliases by the originals (deanonimization)')
    args = parser.parse_args()
    return args

def main():
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
            info = process_doc(raw_text, boxer, depTree, replace = args.replace_entities)
            doc_list.append(info)
        if args.break_output and (count+1) % args.break_output == 0: #save to files
            with open(out_format %out_count, 'w') as out:
                json.dump(doc_list, out)
            out_count += 1
            doc_list = []
        if not args.quiet: print "%6.2f%%" % (100*float(count+1)/length,) #print progress

    if args.break_output:
        if length % args.break_output != 0:
                with open(out_format %out_count, 'w') as out:
                    json.dump(doc_list, out)
                if not args.quiet: print "%6.2f%%" % (100*float(count+1)/length,) #print progress
    else:
        with open(args.out_file, 'w') as out:
             json.dump(doc_list, out)

if __name__ == '__main__':
    main()
