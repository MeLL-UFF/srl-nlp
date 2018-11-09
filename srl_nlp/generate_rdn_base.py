#!/bin/env python2

"""
Generate the rdn Knowledge bases in the appropriate format
"""
from __future__ import division

import logging
import pickle
import random
import re
from multiprocessing.pool import Pool
from os import path, makedirs, listdir
from sys import argv as _argv

from typing import List, Iterator, Dict, Tuple

from srl_nlp.analysers.boxer import BoxerLocalAPI
from srl_nlp.framenet.adapter import PARSERS_AVAILABLE
from srl_nlp.framenet.corpus import Document, Sentence, AnnotationSet
from srl_nlp.logical_representation.logicalform import LF
from srl_nlp.rule_utils import ConfigParser, get_chunks

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


class DataObject(object):

    def __init__(self, s_id, frs, fes, preds):
        # type: (object, List[LF], List[LF], List[LF]) -> None
        self.s_id = s_id
        self.frs = frs
        self.fes = fes
        self.preds = preds

    def __repr__(self):
        return "DataObject({})".format(self.s_id)


def get_annotations(sentence, s_id, var2pos):
    # type: (Sentence, str, Dict[LF,list]) -> Tuple[list, list]
    """
        Get the annotations from the corpus sentence

    Args:
        sentence: Sentence object from a parsed corpus
        s_id: string used to identify the LFs generatedd
        var2pos: map from LFs to lists of variables

    Returns:
        Two lists.
        The first list contains the frame matching, the second one are the fe matching
    """
    logger.debug("Sentence: {sent}".format(sent=sentence))
    fr = []
    fe = []
    inv_mapping = {v: k for (k, v_list) in var2pos.items() for v in v_list}
    for anno_set in sentence:
        if anno_set.is_frame():
            for layer in anno_set:
                for anno in layer:
                    var_list = [inv_mapping[(start, end)] for (start, end) in inv_mapping if
                                start >= anno.start and end <= anno.end]
                    frame_name = anno_set.frameName.encode('string-escape')
                    if layer.name == 'Target':
                        for var in var_list:
                            fr.append(LF("frame_token({S}, '{X}', '{F}').".format(S=s_id, X=var, F=frame_name)))
                    elif layer.name == 'FE':
                        for var in var_list:
                            name = anno.name.encode('string-escape')
                            fe.append(LF(
                                "frame_element_token({S}, {X}, '{FE}', '{F}').".format(S=s_id, X=var, FE=name,
                                                                                       F=frame_name)))
    return fr, fe


# TODO adapted from fsparsing.py
def get_matching_variables(analyser, sentence_str, sentence_lfs=None, matching=None):
    # type: (BoxerLocalAPI, str, List[LF],Dict[any,any]) -> Dict[LF,List[any]]
    if not sentence_lfs:
        sentence_lfs = analyser.sentence2LF(sentence_str)
    if not matching:
        matching = analyser.get_matching_tokens(sentence_str, output="pos")
    matching1 = dict()
    for lf in sentence_lfs:
        for pred in lf.split():
            for child in pred.iterterms():
                if child.get_pred() in matching:
                    term = pred.iterterms().next().get_pred()
                    tmp_list = matching1.get(term, [])
                    tmp_list.append(matching[child.get_pred()])
                    matching1[term] = tmp_list
                    break
    return matching1


def examples_from_doc(boxer, d_id, doc):
    # type: (BoxerLocalAPI, int, Document) -> Iterator[DataObject]
    """

    Args:
        boxer: BoxerAbstract object
        d_id: document id
        doc: document from a Corpus object

    Returns:
        Yields a DataObject
    """
    logger.info("Reading docs")

    for paragraph in doc.elements:
        logger.info("\t Reading paragraph id({par_id})".format(par_id=paragraph.id))

        for sentence in paragraph.sentences:
            logger.info("\t\t Reading sentence id({par_id}:{sent_id})"
                        .format(par_id=paragraph.id, sent_id=sentence.id))
            var2pos = get_matching_variables(boxer, sentence.text)

            try:
                s_id = 's{d:03}{p}{s}'.format(d=d_id, p=paragraph.id, s=sentence.id)
                lfs = boxer.sentence2LF(sentence.text)
                frs, fes = get_annotations(sentence, s_id, var2pos=var2pos)
                preds = []

                for lf in lfs:
                    for pred in lf.split():
                        pred.info.insert(1, [s_id])
                        preds.append(pred)
                logger.info("\t\t Sentence({par_id}:{sent_id}): len(frs)={frs}, len(fes)={fes}, len(preds)={preds}"
                            .format(par_id=paragraph.id, sent_id=sentence.id, frs=len(frs), fes=len(fes),
                                    preds=len(preds)))
                yield DataObject(s_id, frs, fes, preds)
            except (AssertionError, IOError) as ex:
                logger.error(ex.message)
            # break


def write_to_file(root_folder, dataset, dataset_name):
    # type: (str, List[DataObject], str) -> None
    def get_vars(fes):
        for frame_element in fes:
            line = str(frame_element)
            logger.debug("[{ds}] Line: '{line}' was parsed"
                         .format(line=line, ds=dataset_name))
            pattern = re.compile(r'(frame_element_token\(s.*?,)(c\d+)(.*)')
            match = pattern.match(line)
            if match:
                yield match.group(2)
            else:
                logger.warning("[{ds}] Line: '{line}' is not a FET"
                               .format(line=line, ds=dataset_name))

    def replace_var_iter(string, var_set):
        pattern = re.compile(r'(frame_element_token\(s.*?,)(c\d+)(.*)')
        match = pattern.match(string)
        if match:
            for var in var_set.difference({match.group(2)}):
                yield "".join([match.group(1), var, match.group(3)])

    folder = path.join(root_folder, dataset_name)
    logger.info('Root: {f}'.format(f=folder))
    if not path.exists(folder):
        makedirs(folder)
    with open(path.join(folder, dataset_name + '_facts.txt'), 'w') as fact_f:
        with open(path.join(folder, dataset_name + '_pos.txt'), 'w') as pos_f:
            with open(path.join(folder, dataset_name + '_neg.txt'), 'w') as neg_f:
                for data_obj in dataset:
                    # FACTS
                    logger.info('Writing {ds} facts'.format(ds=dataset_name))
                    fact_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    fact_f.write("\n".join(map(str, data_obj.frs)))
                    fact_f.write("\n")
                    fact_f.write("\n".join(map(str, data_obj.preds)))
                    fact_f.write("\n\n")

                    # POSITIVE EXAMPLES
                    logger.info('Writing {ds} positive examples'.format(ds=dataset_name))
                    pos_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    pos_f.write("\n".join(map(str, data_obj.fes)))
                    pos_f.write("\n\n")

                    # NEGATIVE EXAMPLES
                    all_vars = set(get_vars(data_obj.fes))
                    logger.info('Writing {ds} negative examples'.format(ds=dataset_name))
                    neg_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    for fe in data_obj.fes:
                        for out in replace_var_iter(str(fe), all_vars):
                            neg_f.write(out)
                            neg_f.write('\n')
                        pos_f.write("\n")


def _get_examples_from_doc_worker(docs):
    examples = []
    for d_id, doc in enumerate(docs):
        logger.info("Initializing boxer")
        boxer = BoxerLocalAPI()
        logger.info("Processing document {idx}/{total}: '{name}' \n"
                    "From corpus {c_idx}: '{corpus}'".format(idx=d_id + 1, name=doc.name, total=len(docs),
                                                             c_idx=doc.corpusID, corpus=doc.corpus))
        for example in examples_from_doc(boxer, d_id=d_id, doc=doc):
            examples.append(example)
    return examples


def get_examples(data_base_path, input_parser, num_cpus=1):
    if path.isfile(data_base_path):
        with open(data_base_path) as db_file:
            docs = list(input_parser.parse_file(db_file))
    else:
        docs = []
        for f_basename in listdir(data_base_path):
            f_name = path.join(data_base_path, f_basename)
            if path.isfile(f_name):
                with open(f_name) as db_file:
                    docs.extend(input_parser.parse_file(db_file))
    logger.info('Done parsing')
    logger.info('Creating base with {} cpus'.format(num_cpus))
    pool = Pool(num_cpus)
    examples = []
    for example_list in pool.map(_get_examples_from_doc_worker, get_chunks(docs, num_cpus)):
        examples.extend(example_list)
    logger.info('Base done')
    return examples


if __name__ == '__main__':
    import argparse
    from logger_config import add_logger_args as _add_logger_args, config_logger, timeit


    def parse_args(argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Generates RDN Knowledge base')
        parser.add_argument('data_base_path',
                            help='path to the semeval dataset folder')
        parser.add_argument('-i', '--input_format',
                            choices=PARSERS_AVAILABLE.keys(),
                            default='framenet',
                            help='input annotation file format')
        parser.add_argument('-R', '--root', default=".",
                            help='relative path to path to store the bases')
        parser.add_argument('-p', '--pickle_file',
                            help='store intermediate results or read from it if file exists')

        parser.add_argument('-t', '--train_ratio', default=None, type=int,
                            help='optional train/test ratio')
        parser.add_argument('-s', '--seed', default=None, type=int,
                            help='optional random seed')
        parser.add_argument('--set_name', default='train', type=str,
                            help='train dataset name')
        parser.add_argument('--num_cpus', default=1, type=int,
                            help='optional number of cpus')
        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        if args.num_cpus < 1:
            raise ValueError("Number of cpus used should be bigger than zero")
        return args


    @timeit
    def main(argv):
        args = parse_args(argv, _add_logger_args)
        config_logger(args)

        logger.info(str(args))
        logger.info('Initialization')

        # Select the appropriate document parsers for the input and output
        input_parser = PARSERS_AVAILABLE[args.input_format]()
        data_path = path.expandvars(args.data_base_path)
        root_path = path.expandvars(args.root)

        logger.info('Parsing {file}'.format(file=args.data_base_path))

        if args.pickle_file:
            pickle_path = path.expandvars(args.pickle_file)
            if path.exists(pickle_path):
                # Load examples from pickle file
                logger.info('Loading examples from {file}'.format(file=pickle_path))
                with open(pickle_path, 'r') as f:
                    examples = pickle.load(f)
            else:
                # Parse corpus
                examples = get_examples(data_path, input_parser, num_cpus=args.num_cpus)
                # Save pickle file
                with open(pickle_path, 'w') as f:
                    pickle.dump(examples, f)
        else:
            # Parse corpus
            examples = get_examples(data_path, input_parser, num_cpus=args.num_cpus)

        # Split dataset in training and test if train_ratio is given
        if args.train_ratio:
            # shuffle examples
            random.Random(args.seed).shuffle(examples)

            len_train = (args.train_ratio * len(examples) / (args.train_ratio + 1))
            train, test = examples[:len_train], examples[len_train:]
            write_to_file(root_path, train, 'train')
            write_to_file(root_path, test, 'test')
        else:
            write_to_file(root_path, examples, args.set_name)
        logger.info('Done')


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
        exit(1)
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
