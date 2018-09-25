#!/bin/env python2

"""

"""

from sys import argv as _argv

import logging
from ConfigParser import ConfigParser
from os import path
from typing import Dict, Tuple

from srl_nlp.analysers.boxer import BoxerLocalAPI
from srl_nlp.framenet.adapter import PARSERS_AVAILABLE
from srl_nlp.framenet.corpus import Paragraph, Document, Sentence
from srl_nlp.fsparsing import PrologBoxerAnnotator
from srl_nlp.logical_representation.logicalform import LF

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


def get_annotations(sentence, lf, var2pos):
    # type: (Sentence, LF, Dict [str,str]) -> Tuple [list, list]
    """
    Get the annotations from the corpus sentence

    Args:
        sentence:
        lf:
        var2pos:

    Returns: Two lists. The first list contains the frame matching, the second one are the fe matching
    """

    logger.debug("Sentence: {sent}, {lf}".format(sent=sentence, lf=lf))
    fr = []
    fe = []
    inv_mapping = {v: k for (k, v_list) in var2pos.items() for v in v_list}
    for anno_set in sentence:
        for layer in anno_set:
            for anno in layer:
                var_list = [inv_mapping[(start, end)] for (start, end) in inv_mapping if
                            start >= anno.start and end <= anno.end]
                frame_name = anno_set.frameName.lower()
                if layer.name == 'Target':
                    for var in var_list:
                        fr.append(LF('frame_related({X}, {F}).'.format(X=var, F=frame_name)))
                elif layer.name == 'FE':
                    for var in var_list:
                        name = anno.name.lower()
                        fe.append(LF('frame_element({X}, {FE}, {F}).'.format(X=var, FE=name, F=frame_name)))
    return fr, fe


def remove_fe_equal_to_f_from_sentence(sentence):
    # type: (Sentence) -> None
    fr_pos_list = [(anno.start, anno.end)
                   for anno_set in sentence
                   for layer in anno_set
                   for anno in layer
                   if layer.name == 'Target']
    for anno_set in sentence:
        for layer in anno_set:
            if layer.name == 'FE':
                layer.annotations = [anno for anno in layer if not (anno.start, anno.end) in fr_pos_list]
        anno_set.layers = [layer for layer in anno_set if len(layer) > 0]


def eval_corpus(annotator, boxer, docs, skip_frame_matching=False):
    out_docs = []
    logger.info("Reading docs")
    for doc in docs:
        out_doc = Document(doc_id=doc.id, corpus_id=doc.corpusID, name=doc.name, desc=doc.desc)

        for paragraph in doc:
            logger.info("Reading paragraph id({par_id})".format(par_id=paragraph.id))
            out_paragraph = Paragraph(paragraph_id=paragraph.id)

            for sentence in paragraph:
                logger.info("Reading sentence id({par_id}:{sent_id})"
                            .format(par_id=paragraph.id, sent_id=sentence.id))

                out_sentence = Sentence(sent_id=sentence.id, text=sentence.text,
                                        parts_of_speech=sentence.parts_of_speech)

                var2pos = annotator.get_matching_variables(sentence.text)
                try:
                    if skip_frame_matching:
                        frs, _ = get_annotations(sentence, boxer.sentence2LF(sentence.text), var2pos=var2pos)
                        matching, err = annotator.frame_element_matching(sentence.text, fr_anno=frs, out_error=True)
                        matching.extend(frs)
                    else:
                        matching, err = annotator.matching(sentence.text, out_error=True)

                    logger.debug(err)
                    matching = set(matching)

                    for f_name, annoset in annotator.sem_annotations(sentence.text, anno_lfs=matching,
                                                                     token2pos=var2pos).iteritems():
                        out_sentence.annotation_sets.append(annoset)

                    remove_fe_equal_to_f_from_sentence(out_sentence)
                    out_paragraph.sentences.append(out_sentence)

                except (AssertionError, IOError) as ex:
                    logger.error(ex.message)

            out_doc.elements.append(out_paragraph)
        out_docs.append(out_doc)
    return out_docs


if __name__ == '__main__':
    import argparse
    from logger_config import add_logger_args as _add_logger_args, config_logger, timeit


    def parse_args(argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Evaluates the pipeline')
        parser.add_argument('data_base_path',
                            help='path to the semeval dataset folder')

        parser.add_argument('-s', '--skip_frame_matching',
                            action='store_true', default=False,
                            help='skip the frame matching step and use the one from the data')
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

        # Select the appropriate document parsers for the input and output
        input_parser = PARSERS_AVAILABLE[args.input_format]()
        output_parser = PARSERS_AVAILABLE[args.output_format]()

        # Get the paths of the rules
        kb_fr_path = path.join(args.kb_path, args.kb_fr)
        kb_fe_path = path.join(args.kb_path, args.kb_fe)

        annotator = PrologBoxerAnnotator(boxer, kb_fr_path, kb_fe_path)

        logger.info('{anno} is parsing {file}'.format(anno=annotator, file=args.data_base_path))

        # Parse corpus
        with open(args.data_base_path) as db_file:
            docs = input_parser.parse_file(db_file)
        logger.info('Done parsing')

        out_docs = eval_corpus(annotator, boxer, docs, skip_frame_matching=args.skip_frame_matching)

        if args.annotation_output_path:
            logger.info("Storing results at {f_name}".format(f_name=args.annotation_output_path))
            with open(args.annotation_output_path, 'w') as f_out:
                output_parser.write_doc(out_docs[0], f_out)
        else:
            logger.info("Printing results to stdout")
            output_parser.write_doc(out_docs[0])
        logger.info('Done')


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
