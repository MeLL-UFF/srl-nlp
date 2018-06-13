#!/bin/env python2

"""

"""
import argparse
from sys import argv as _argv

from analysers.boxer import BoxerLocalAPI
from framenet.adapter import PARSERS_AVAILABLE
from framenet.corpus import Paragraph, Document, Sentence
from fsparsing import PrologAnnotator
from logger_config import add_logger_args as _add_logger_args, config_logger

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


def parse_args(argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='Evaluates the pipeline')
    # parser.add_argument('dir_path', help = 'the path of the experiments')
    parser.add_argument('data_base_path',
                        help='path to the semeval dataset folder')

    # TODO implement frame matching skip
    # parser.add_argument('-s', '--skip_frame_matching',
    #                     action='store_true', default=False,
    #                     help='skip the frame matching step and use the one from the data')
    parser.add_argument('-a', '--annotation_output_path',
                        help='output file of the annotations')

    parser.add_argument('-i', '--input_format',
                        choices=PARSERS_AVAILABLE.keys(),
                        default=PARSERS_AVAILABLE.keys()[-1],
                        help='input annotation file format')
    parser.add_argument('-o', '--output_format',
                        choices=PARSERS_AVAILABLE.keys(),
                        default=PARSERS_AVAILABLE.keys()[-1],
                        help='output annotation file format')
    parser.add_argument('-K', '--kb_path', default='.',
                        help='path to knowledge base files')
    parser.add_argument('-E', '--kb_fe', default='kb_fe',
                        help='relative path to frame element knowledge base')
    parser.add_argument('-R', '--kb_fr', default='kb_fr',
                        help='relative path to path to frame matching knowledge base')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


@timeit
def main(argv):
    args = parse_args(argv, _add_logger_args)
    config_logger(args)

    logger.info('Initialization')

    boxer = BoxerLocalAPI()

    input_parser = PARSERS_AVAILABLE[args.input_format]()
    output_parser = PARSERS_AVAILABLE[args.output_format]()

    kb_fr_path = path.join(args.kb_path, args.kb_fr)
    kb_fe_path = path.join(args.kb_path, args.kb_fe)

    annotator = PrologAnnotator(boxer, kb_fr_path, kb_fe_path)

    logger.info('{anno} is parsing {file}'.format(anno=annotator, file=args.data_base_path))

    with open(args.data_base_path) as db_file:
        docs = input_parser.parseXML(db_file)
    logger.info('Done parsing')
    out_docs = []

    for doc in docs:
        out_doc = Document(id=doc.id, corpus_id=doc.corpusID, name=doc.name, desc=doc.desc)
        for paragraph in doc:
            out_paragraph = Paragraph(id=paragraph.id)

            for sentence in paragraph:
                out_sentence = Sentence(id=sentence.id, text=sentence.text, parts_of_speech=sentence.parts_of_speech)
                try:
                    matching, err = annotator.matching(sentence.text, out_error=True)
                    matching = set(matching)
                    logger.debug(err)
                    # print '\'%s\n\'' % '\n'.join(map(str, matching))
                    for f_name, annoset in annotator.sem_annotations(sentence.text, matching).iteritems():
                        # print "Frame:{f_name}\n\t{anno}".format(f_name=f_name, anno=input_parser._anno_set2XML(annoset))
                        out_sentence.annotation_sets.append(annoset)
                    out_paragraph.sentences.append(out_sentence)
                except (AssertionError, IOError) as e:
                    logger.error(e.message)

            out_doc.elements.append(out_paragraph)
        out_docs.append(out_doc)

    if args.annotation_output_path:
        with open(args.annotation_output_path, 'w') as f_out:
            output_parser.doc2XML(out_docs[0], f_out)
    else:
        output_parser.doc2XML(out_docs[0])
    logger.info('Done')


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
