#!/bin/env python2

"""
Script for running annotation on the document files.
"""

from sys import argv as _argv

import logging
from configparser import ConfigParser
from os import path
from typing import List

from srl_nlp.framenet.adapter import PARSERS_AVAILABLE, DocumentAdapter
from srl_nlp.framenet.corpus import Paragraph, Document
from srl_nlp.data_augmentation.scripts.fsparsing import Annotator, SemaforAnnotator
from srl_nlp.rule_utils import list_doc_files

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


def annotate_corpus(annotator, docs):
    # type: (Annotator, List[Document]) -> List[Document]
    out_docs = []
    logger.info("Reading docs")

    for doc in docs:
        raw_sentences = [sentence for paragraph in doc for sentence in paragraph]
        annnotated_sentences = annotator.matching(raw_sentences)
        anno_sent_idx = 0

        out_doc = Document(doc_id=doc.id, corpus_id=doc.corpusID, name=doc.name, desc=doc.desc)

        for paragraph in doc:
            logger.info("Reading paragraph id({par_id})".format(par_id=paragraph.id))
            out_paragraph = Paragraph(paragraph_id=paragraph.id)

            for sentence in paragraph:
                logger.info("Reading sentence id({par_id}:{sent_id})".format(par_id=paragraph.id, sent_id=sentence.id))
                anno_sent = annnotated_sentences[anno_sent_idx]
                out_paragraph.sentences.append(anno_sent)
                anno_sent_idx = anno_sent_idx + 1
            out_doc.elements.append(out_paragraph)
        out_docs.append(out_doc)
    return out_docs


if __name__ == '__main__':
    import argparse
    from srl_nlp.logger_config import add_logger_args as _add_logger_args, config_logger, timeit


    def parse_args(argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Evaluates the pipeline')
        parser.add_argument('data_base_path',
                            help='Path to the corpus file or folder containing the corpus files')

        parser.add_argument('-s', '--skip_frame_matching',
                            action='store_true', default=False,
                            help='skip the frame matching step and use the one from the data')
        parser.add_argument('-a', '--annotation_output_path',
                            help='Path to the output corpus file or to the folder where to write the output files')

        parser.add_argument('-i', '--input_format',
                            choices=PARSERS_AVAILABLE.keys(),
                            default=PARSERS_AVAILABLE.keys()[-1],
                            help='Input annotation file format')
        parser.add_argument('-o', '--output_format',
                            choices=PARSERS_AVAILABLE.keys(),
                            default=PARSERS_AVAILABLE.keys()[-1],
                            help='Output annotation file format')
        # TODO see how to insert multiple annotators
        parser.add_argument('-K', '--kb_path', default='.',
                            help='Path to knowledge base files')
        parser.add_argument('-E', '--kb_fe', default='kb_fe',
                            help='Relative path to frame element knowledge base')
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

        # boxer = BoxerLocalAPI()

        # Select the appropriate document parsers for the input and output
        input_parser = PARSERS_AVAILABLE[args.input_format]()  # type: DocumentAdapter
        output_parser = PARSERS_AVAILABLE[args.output_format]()  # type: DocumentAdapter

        # # Get the paths of the rules
        # kb_fr_path = path.join(args.kb_path, args.kb_fr)
        # kb_fe_path = path.join(args.kb_path, args.kb_fe)
        #
        # annotator = PrologBoxerAnnotator(boxer, kb_fr_path, kb_fe_path)

        annotator = SemaforAnnotator()

        logger.info('{anno} is parsing {file}'.format(anno=annotator, file=args.data_base_path))

        # Handle if data_base_path or annotation_output_path are paths or files
        if args.data_base_path is not None and path.isdir(args.data_base_path):
            if args.annotation_output_path and not path.isdir(args.annotation_output_path):
                raise argparse.ArgumentError("If data_base_path is a folder, "
                                             "annotation_output_path should also be a folder and exist.")
            file_names = list_doc_files(args.data_base_path)
            in_files = [path.join(args.data_base_path, f) for f in file_names]
        elif path.isfile(args.data_base_path):
            in_files = [args.data_base_path]
        else:
            raise argparse.ArgumentError(" {} does not exist.".format(args.data_base_path))

        if args.annotation_output_path is None:
            out_files = [None] * len(in_files)
        else:
            if path.isdir(args.annotation_output_path):
                out_files = [path.join(args.annotation_output_path, f) for f in map(path.basename, in_files)]
            else:
                out_files = [args.annotation_output_path]

        for in_file, out_file in zip(in_files, out_files):
            # Parse corpus
            logger.info("\n Parsing {}".format(in_file))
            with open(in_file) as db_file:
                docs = input_parser.parse_file(db_file)
            logger.info('Done parsing')

            out_docs = annotate_corpus(annotator, docs)

            if out_file is not None:
                logger.info("Storing results in {f_name}".format(f_name=out_file))
                with open(out_file, 'w') as f_out:
                    output_parser.write_doc(out_docs[0], f_out)
            else:
                logger.info("Printing results to stdout")
                print(output_parser.doc_to_string(out_docs[0]))
        logger.info('Done')


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
