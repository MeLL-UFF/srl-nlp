from __future__ import division

import sys

import logging
from builtins import zip
from copy import deepcopy as copy
from multiprocessing import Pool
from typing import Tuple, List, Union, Dict, Iterable

from srl_nlp.data_augmentation.analysers.boxer import BoxerAbstract, BoxerLocalAPI
from srl_nlp.data_augmentation.analysers.dependencytree import DependencyTreeLocalAPI
from srl_nlp.framenet.corpus import Document, Paragraph, Sentence, AnnotationSet
from srl_nlp.framenet.description import EXample, FEeXample, Label
from srl_nlp.framenet.framenet import Frame, FrameElement, FrameNet
from srl_nlp.data_augmentation.logical_representation import LF
from srl_nlp.rule_utils import list_doc_files, get_chunks, ensure_dir
from srl_nlp.annotation_utils import get_annotations, get_factors, get_paths

FE = FrameElement
logger = logging.getLogger(__name__)

sys.setrecursionlimit(3000)  # TODO this is a work around to allow pickle to work on the FN data object


class StandardAugmentation(object):
    STRONG_RELATIONS = ('Is Inherited by', 'Subframe of')
    WEAK_RELATIONS = ('Is Causative of', 'Is Inchoative of', 'Is Perspectivized in', 'Is Preceded by',
                      'Is Used by', 'Perspective on', 'Precedes', 'Uses')

    ALL_RELATIONS = ('Has Subframe(s)', 'Inherits from', 'Is Inherited by', 'Subframe of', 'See also',
                     'Is Causative of', 'Is Inchoative of', 'Is Perspectivized in', 'Is Preceded by', 'Is Used by',
                     'Perspective on', 'Precedes', 'Uses')

    def __init__(self):
        self.ex_count = 0
        self.doc_ex_count = 0
        self.total_sentences = 0

    def augment_framenet(self, fn, ignored_relations=tuple()):
        # type: (FrameNet, Tuple[str]) -> FrameNet
        self.ex_count = 0
        self.doc_ex_count = 0
        self.total_sentences = 0
        if len(ignored_relations) == 0:
            new_frames = [self.augment_frame(frame, fn) for frame in fn]
        else:
            new_frames = [self.augment_frame(frame, fn, ignored_relations=ignored_relations) for frame in fn]
        fn_augmented = FrameNet(new_frames)
        logger.info('There were added {} fn examples'.format(self.ex_count))
        return fn_augmented

    def augment_frame(self, frame_or_frame_name, fn, ignored_relations=('See also',), sentence=None):
        # type: (Union[Frame, str], FrameNet, Iterable[str], Sentence) -> object
        if isinstance(frame_or_frame_name, str):
            frame = fn[frame_or_frame_name]
        else:
            frame = frame_or_frame_name

        neighbour_frames = self.get_neighbours(frame, ignored_relations)
        core_fes, peripheral_fes = self._augment_frame_aux(frame.coreFEs, frame.peripheralFEs, neighbour_frames,
                                                           sentence)

        return Frame(name=frame.name, description=frame.description, idx=frame.id, lus=frame.LUs,
                     core_fes=core_fes, peripheral_fes=peripheral_fes, **frame.relations)

    @staticmethod
    def get_neighbours(frame, ignored_relations):
        # type: (Frame, Iterable[str]) -> List[Frame]
        neighbour_frames = [neighbour_frame for name, relation in frame.relations.items()
                            for neighbour_frame in relation
                            if name not in ignored_relations]
        return neighbour_frames

    def _augment_frame_aux(self, core_fes, peripheral_fes, neighbour_frames, sentence=None):
        # type: (List[FE], List[FE], List[Frame], Sentence) -> Tuple[List[FE], List[FE]]

        core_fes = copy(core_fes)
        peripheral_fes = copy(peripheral_fes)

        for neighbour_frame in neighbour_frames:
            dict_fes = self._get_fe_dict(core_fes + peripheral_fes, neighbour_frame.fes, sentence=sentence)
            for fe in core_fes:
                if fe in dict_fes:
                    for new_example in dict_fes[fe].definition.get_elements(EXample):
                        try:
                            converted_example = self._convert_fn_example(new_example, dict_fes, invert_dict=True)
                            fe.definition.add_element(converted_example)
                            self.ex_count = self.ex_count + 1
                        except KeyError as err:
                            logger.warning('Missing fe {}'.format(err))
        return core_fes, peripheral_fes

    def _get_fe_dict(self, fe_list_old, fe_list_new, sentence=None):
        # type: (List[FE], List[FE], Sentence) -> Dict[FE,FE]
        """
            Returns a Dictionary that matches the frame elements in fe_list_old to the frame elements in fe_list_new
            based on the frame elements name equality or based on the similarity function _fes_are_similiar
        Args:
            fe_list_old: list of frame elements to use as key of the dictionary
            fe_list_new: list of frame elements to use as value of the dictionary
            sentence: optional argument used to help the similarity algorithm

        Returns:
            A dictionary that matches frame elements from a list to their related frame elements in the other list
        """
        dict_fes = {fe: None for fe in fe_list_old}
        for other_fe in fe_list_new:  # Test for name equality
            for fe in dict_fes:
                if dict_fes[fe] is None:
                    if fe == other_fe:
                        dict_fes[fe] = other_fe
                        break
                else:
                    break
        for other_fe in fe_list_new:  # Test for similarity
            for fe in dict_fes:
                if dict_fes[fe] is None:
                    if self._fes_are_similiar(fe, other_fe, sentence):
                        dict_fes[fe] = other_fe
                        break
                else:
                    break
        return {k: v for k, v in dict_fes.items() if v is not None}

    def _fes_are_similiar(self, fe, other_fe, sentence=None):
        # logger.debug("{} not similar to {}".format(fe, other_fe))
        return False

    @staticmethod
    def _convert_fn_example(example, dict_fes, invert_dict=False):
        # type: (Label, Dict[FE,FE], bool) -> Label
        if invert_dict:
            dict_fes = {v: k for k, v in dict_fes.items()}  # type: Dict[FE,FE]
        name_abbrev2fe = {fe_from.name: fe_to for fe_from, fe_to in dict_fes.items()}  # type: Dict[str,FE]
        name_abbrev2fe.update({fe_from.abbrev: fe_to for fe_from, fe_to in dict_fes.items()})

        new_example = copy(example)
        for element in iter(new_example):
            if isinstance(element, FEeXample):
                fex = element  # type: FEeXample
                fex.attribs['name'] = name_abbrev2fe[fex.attribs['name']].abbrev
        return new_example

    def augment_documents(self, docs, fn, ignored_relations=None, split_in_sentences=True):
        # type: (List[Document], FrameNet, Tuple[str],bool) -> List[Document]
        """
        Returns a list of augmented documents
        Args:
            docs: a list of Document instances to be augmented
            fn: fn: the FrameNet structure used to support the augmentation
            ignored_relations: Tuple of string that represent the relations to be ignored during augmentation
            split_in_sentences: each new annotation generated by the augmentation will be written in a copy of the
                                original sentence. (This increases the number of sentences in the documents)

        Returns:
            New Document object list with same info as docs but all the sentences were augmented
        """
        self.doc_ex_count = 0
        self.total_sentences = 0
        if docs is not None:
            logger.info('Augmenting documents')
            for i, doc in enumerate(docs):
                logger.info('Augmenting {}-th doc'.format(i))
                if len(ignored_relations) == 0:
                    yield self.augment_document(doc, fn, split_in_sentences=split_in_sentences)
                else:
                    yield self.augment_document(doc, fn,
                                                ignored_relations=ignored_relations,
                                                split_in_sentences=split_in_sentences)
            logger.info('There were added {} doc sentence examples'.format(self.doc_ex_count))
            logger.info('There were {} doc sentence examples'.format(self.total_sentences))

    def augment_document(self, doc, fn, ignored_relations=('See also',), split_in_sentences=True):
        # type: (Document, FrameNet, Iterable[str],bool) -> Document
        """
        Returns an augmented document
        Args:
            doc: an Document instance to be augmented
            fn: fn: the FrameNet structure used to support the augmentation
            ignored_relations: Tuple of string that represent the relations to be ignored during augmentation
            split_in_sentences: each new annotation generated by the augmentation will be written in a copy of the
                                original sentence.
                                (This increases the number of sentences in the document)

        Returns:
            New Document object with same info as doc but all the sentences were augmented
        """
        new_elements = []
        for i, element in enumerate(doc):
            if isinstance(element, Paragraph):
                # new_paragraph = copy(element)  # type: Paragraph
                new_paragraph = Paragraph(paragraph_id=element.id)  # type: Paragraph
                logger.info('\t Augmenting {} of {} paragraphs'.format(i + 1,
                                                                       len(doc.elements)))
                for j, sentence in enumerate(element):
                    logger.info('\t\t Augmenting {} of {} sentences'.format(j + 1,
                                                                            len(element.sentences)))
                    if split_in_sentences:
                        new_paragraph.sentences.append(copy(sentence))
                        new_sentences = self.augment_doc_sentence(sentence, fn, ignored_relations=ignored_relations)
                        new_paragraph.sentences.extend(new_sentences)
                    else:
                        new_sentence = copy(sentence)
                        self.augment_doc_sentence_inplace(new_sentence, fn, ignored_relations=ignored_relations)
                        new_paragraph.sentences.append(new_sentence)
                new_elements.append(new_paragraph)
            elif isinstance(element, Sentence):
                logger.info('\t Augmenting {} of {} sentences'.format(i, len(doc.elements)))
                sentence = element
                if split_in_sentences:
                    new_elements.append(copy(sentence))
                    new_sentences = self.augment_doc_sentence(sentence, fn, ignored_relations=ignored_relations)
                    new_elements.extend(new_sentences)
                else:
                    new_sentence = copy(sentence)  # type: Sentence
                    self.augment_doc_sentence_inplace(new_sentence, fn, ignored_relations=ignored_relations)
                    new_elements.append(new_sentence)
            else:
                logger.warning("Strange doc element, skipping it")
        new_doc = Document(doc_id=doc.id,
                           corpus=doc.corpus,
                           corpus_id=doc.corpusID,
                           name=doc.name,
                           desc=doc.desc,
                           elements=new_elements)
        return new_doc

    def augment_doc_sentence_inplace(self, sentence, fn, ignored_relations=('See also',)):
        # type: (Sentence, FrameNet, Iterable[str]) -> None
        new_anno_sets = []
        self.total_sentences = self.total_sentences + 1

        sentence_frames = set()

        for annotation_set in sentence:
            # If the frame in the annotation is not present in the FrameNet then skip this annotation
            if annotation_set.is_frame() and annotation_set.frameName in fn:
                sentence_frames.add(annotation_set.frameName)

        for i, annotation_set in enumerate(sentence):
            # If the frame in the annotation is not present in the FrameNet then skip this annotation
            if not annotation_set.is_frame():
                continue
            if annotation_set.frameName not in fn:
                logger.warning("Frame '{}' not found in FN".format(annotation_set.frameName))
            else:
                frame = fn[annotation_set.frameName]
                neighbour_frames = self.get_neighbours(frame, ignored_relations)
                fes = [fn.get_frame_element(layer.name)
                       for layer in annotation_set.get_fes()
                       if fn.has_frame_element(layer.name)]

                logger.info('\t\t\t Processing {:2d} of {} '
                            'annotation sets in sentence'.format(i + 1, len(sentence.annotation_sets)))
                for neighbour_frame in neighbour_frames:
                    if neighbour_frame.name not in sentence_frames:
                        fes_dict = self._get_fe_dict(fes, neighbour_frame.fes, sentence=sentence)
                        if len(fes_dict) > 0 and len(fes_dict) == len(fes):
                            # if this condition is met, then we can make the transference:
                            self.doc_ex_count = self.doc_ex_count + 1
                            new_anno_sets.append(self.augment_doc_anno_set(annotation_set,
                                                                           neighbour_frame.name,
                                                                           neighbour_frame.id,
                                                                           fes_dict))
                            sentence_frames.add(neighbour_frame.name)  # store that this frame was added to the sentence

        sentence.annotation_sets.extend(new_anno_sets)
        return None

    def augment_doc_sentence(self, sentence, fn, ignored_relations=('See also',)):
        # type: (Sentence, FrameNet, Iterable[str]) -> List[Sentence]
        """
        Generates a list of Sentences with the results of the augmentation

        Args:
            sentence:  the sentence to be augmented
            fn: the FrameNet structure used to support the augmentation
            ignored_relations: Frame relations to be ignored during augmentation

        Returns:
            A list of new sentences where each sentence is a copy of the given sentence with augmented frame element
            annotations from a related frame

        """
        self.total_sentences = self.total_sentences + 1
        new_sentences = []

        sentence_frames = set()

        for annotation_set in sentence:
            # If the frame in the annotation is not present in the FrameNet then skip this annotation
            if annotation_set.is_frame() and annotation_set.frameName in fn:
                sentence_frames.add(annotation_set.frameName)

        for i, annotation_set in enumerate(sentence):
            # If the frame in the annotation is not present in the FrameNet then skip this annotation
            if not annotation_set.is_frame():
                continue
            if annotation_set.frameName not in fn:
                logger.warning("Frame '{}' not found in FN".format(annotation_set.frameName))
            else:
                frame = fn[annotation_set.frameName]
                neighbour_frames = self.get_neighbours(frame, ignored_relations)
                fes = [fn.get_frame_element(layer.name)
                       for layer in annotation_set.get_fes()
                       if fn.has_frame_element(layer.name)]

                logger.info('\t\t\t Processing {:2d} of {} '
                            'annotation sets in sentence'.format(i + 1, len(sentence.annotation_sets)))
                for neighbour_frame in neighbour_frames:
                    if neighbour_frame.name not in sentence_frames:
                        fes_dict = self._get_fe_dict(fes, neighbour_frame.fes, sentence=sentence)
                        if len(fes_dict) > 0 and len(fes_dict) == len(fes):
                            # if this condition is met, then we can make the transference:
                            self.doc_ex_count = self.doc_ex_count + 1
                            new_sentence = copy(sentence)
                            new_sentence.annotation_sets[i] = self.augment_doc_anno_set(annotation_set,
                                                                                        neighbour_frame.name,
                                                                                        neighbour_frame.id,
                                                                                        fes_dict)
                            new_sentence.id = "{}{}".format(len(new_sentences) + 1, sentence.id)
                            new_sentences.append(new_sentence)
                            sentence_frames.add(neighbour_frame.name)  # store that this frame was added to the sentence
        return new_sentences

    @staticmethod
    def augment_doc_anno_set(annotation_set, frame_name, frame_idx, fes_dict):
        # type: (AnnotationSet, str, Union[int, str], Dict[FE,FE]) -> AnnotationSet
        str2fe = {fe_from.name: fe_to for fe_from, fe_to in fes_dict.items()}  # type: Dict[FE, FE]
        new_annotation_set = copy(annotation_set)
        new_annotation_set.frameName = frame_name
        new_annotation_set.frameID = frame_idx
        for layer in new_annotation_set:
            if layer.name.lower() == 'fe':
                for annotation in layer:
                    other_fe = str2fe[annotation.name]
                    annotation.name = other_fe.name
                    annotation.idx = other_fe.id
            if layer.name.lower() == 'target':
                pass
        return new_annotation_set


class AnalyserAugmentation(StandardAugmentation):

    def __init__(self, analyser):
        super(AnalyserAugmentation, self).__init__()
        self.analyser = analyser
        self._curr_sentence = None
        self._fe_to_path = dict()

        if isinstance(self.analyser, BoxerAbstract):
            self.lemmatizer = None
        else:
            self.lemmatizer = lambda x: x

        try:
            self._name = str(self.analyser.__class__).split('\'')[1].split('.')[-1]
        except IndexError:
            self._name = str(self.analyser.__class__)

    def __str__(self):
        return "AnalyserAugmentation[{}]".format(self._name)

    def __repr__(self):
        return str(self)

    def _fes_are_similiar(self, l_fe, r_fe, sentence=None):
        # type: (FE, FE, Sentence) -> bool
        if sentence != self._curr_sentence:
            self._curr_sentence = sentence
            self._fe_to_path = dict()
        for fe_example in l_fe.definition.get_elements(EXample):
            fe_paths = self._get_path(fe_example, abbrev2fe_name={l_fe.abbrev: l_fe.name})
            for other_fe_example in r_fe.definition.get_elements(EXample):
                other_fe_paths = self._get_path(other_fe_example, abbrev2fe_name={r_fe.abbrev: r_fe.name})
                for fe_path in fe_paths:
                    for other_fe_path in other_fe_paths:
                        if self.compare_paths(fe_path, other_fe_path):
                            return True
        logger.debug("{} not similar to {}".format(l_fe, r_fe))
        return False

    def _get_path(self, fe_example, abbrev2fe_name=None, update_dict=True):
        if fe_example not in self._fe_to_path:
            if update_dict:
                self._fe_to_path[fe_example] = self.list_all_paths(fe_example, abbrev2fe_name=abbrev2fe_name)
            else:
                return self.list_all_paths(fe_example, abbrev2fe_name=abbrev2fe_name)
        return self._fe_to_path[fe_example]

    def list_all_paths(self, fe_example, abbrev2fe_name=None):
        # type: (EXample, Dict[str,str]) -> List[List[LF]]
        """
        Lists all paths found between a predicate and the target
        Args:
            fe_example:
            abbrev2fe_name: dictionary that maps abbreviations to frame names

        Returns:
            List of paths
        """
        fe_paths = []
        fe_lfs = self.analyser.sentence2LF(fe_example.str_no_annotation())
        if len(fe_lfs) > 0:
            fe_lf = fe_lfs[0]
            elements, targets = get_annotations(fe_example, fe_lf, abbrev2fe=abbrev2fe_name, get_lemma=self.lemmatizer)
            factors = get_factors(fe_lf)

            for target in targets:
                for label, preds in elements.items():
                    if label[:1].islower():
                        logger.warning(Exception(
                            '"{label}" FE was not matched, the label is lower cased\n'
                            'dict: {dict}'.format(label=label, dict=abbrev2fe_name)))
                    else:
                        for pred in preds:
                            for term_path in get_paths(pred, target, factors):
                                # logger.info(str(term_path))
                                fe_paths.append([target] + term_path)
        return fe_paths

    @staticmethod
    def compare_paths(l_fe_path, r_fe_path):
        # type: (List[LF], List[LF]) -> bool
        if len(l_fe_path) != len(r_fe_path):
            return False

        for l_pred, r_pred in zip(l_fe_path, r_fe_path):
            if l_pred.get_pred() != r_pred.get_pred():
                return False
        return True


class AugmentationQueue(StandardAugmentation):

    def __init__(self, *augmentations):
        # type: (List[Union[StandardAugmentation, str]]) -> None
        super(AugmentationQueue, self).__init__()
        self.augmentations = augmentations  # type: List[StandardAugmentation]

    def _fes_are_similiar(self, l_fe, r_fe, sentence=None):
        # type: (FE, FE, Sentence) -> bool

        for method in self.augmentations:
            if method._fes_are_similiar(l_fe, r_fe, sentence):
                return True
        return False


class AugmentationFactory:

    def __init__(self):
        pass

    @property
    def lexical(self):
        return StandardAugmentation()

    @property
    def syntactic(self):
        return AnalyserAugmentation(DependencyTreeLocalAPI())

    @property
    def semantic(self):
        return AnalyserAugmentation(BoxerLocalAPI())

    @property
    def full(self):
        return AugmentationQueue(AnalyserAugmentation(DependencyTreeLocalAPI()),
                                 AnalyserAugmentation(BoxerLocalAPI()))

    def get_augmentation(self, aug_name):
        if isinstance(getattr(AugmentationFactory, aug_name), property):
            return getattr(self, aug_name)


def _augment_aux(params):
    # type: (Tuple[str, List[Document], FrameNet, Tuple[str], bool]) -> List[Document]
    aug_name, doc_list, fnet, ignored, split_in_sentences = params
    aug_method = getattr(AugmentationFactory(), aug_name)  # type: StandardAugmentation
    if ignored is None:
        return list(aug_method.augment_documents(doc_list, fnet, split_in_sentences=split_in_sentences))
    else:
        return list(aug_method.augment_documents(doc_list, fnet,
                                                 ignored_relations=ignored,
                                                 split_in_sentences=split_in_sentences))


class ParallelAugmentation(StandardAugmentation):

    def __init__(self, num_cpus, aug_name):
        # type: (int, str) -> None
        super(ParallelAugmentation, self).__init__()
        self._aug_name = aug_name
        self._num_cpus = num_cpus

    def augment_documents(self, docs, fn, ignored_relations=tuple(), split_in_sentences=True):
        # type: (List[Document], FrameNet, Iterable[str],bool) -> List[Document]
        self.doc_ex_count = 0
        self.total_sentences = 0

        logger.info('Augmenting documents using {} cpus'.format(self._num_cpus))
        pool = Pool(self._num_cpus)

        jobs = [(self._aug_name, doc_list, fn, ignored_relations, split_in_sentences)
                for doc_list in get_chunks(docs, self._num_cpus)]

        for i, docs in enumerate(pool.map(_augment_aux, jobs)):
            logger.info('Augmenting {}-th doc'.format(i))
            for doc in docs:
                yield doc


if __name__ == '__main__':
    from srl_nlp.framenet.adapter import PARSERS_AVAILABLE, DocumentAdapter
    from srl_nlp.logger_config import add_logger_args, config_logger, timeit
    from srl_nlp.framenet.parse_xml import NetXMLParser
    from itertools import chain
    from sys import argv as _argv
    from os import path
    import argparse

    augmentations_available = [aug for aug in dir(AugmentationFactory)
                               if isinstance(getattr(AugmentationFactory, aug), property)]

    relations_map = {'STRONG': StandardAugmentation.STRONG_RELATIONS,
                     'WEAK': StandardAugmentation.WEAK_RELATIONS,
                     'ALL': StandardAugmentation.ALL_RELATIONS,
                     'NONE': tuple(),

                     'subframe': ('Subframe of',),
                     'inheritance': ('Is Inherited by',),

                     'causative': ('Is Causative of',),
                     'perspective': ('Perspective on',),
                     'precedence': ('Precedes',),
                     'use': ('Uses', 'Is Used by'),
                     'see_also': ('See also',)}


    def get_docs_and_names(adapter, file_folder_path):
        # type: (DocumentAdapter, str) -> Tuple[List[Document], List[str]]
        docs = []  # type: List[Document]
        if path.isfile(file_folder_path):
            docs.extend(adapter.parse_file(file_folder_path))
            doc_names = [path.basename(file_folder_path)]
        else:
            doc_names = list(list_doc_files(file_folder_path))
            for doc_name in doc_names:
                docs.extend(adapter.parse_file(path.join(file_folder_path, doc_name)))
        return docs, doc_names


    def parse_args(argv):
        parser = argparse.ArgumentParser(
            description='Runs augmentation of the given documents')
        parser.add_argument('framenet_path', default=None,
                            help='the path to the folders with the documents or to the document')
        parser.add_argument('augmentation_types', nargs='+', choices=augmentations_available,
                            help='The kind of augmentations used, if multiple augmentations, a folder for each '
                                 'augmentation will be generated in the specified doc_paths.')
        parser.add_argument('--doc_paths', default=list(), nargs=2,
                            help='Two paths, the first one is the folder or document from where we will retrieve the '
                                 'document information. '
                                 'The second one is the path to where store the augmented information')
        parser.add_argument('--document_adapter', default='framenet', choices=PARSERS_AVAILABLE,
                            help='')
        parser.add_argument('--save_framenet_path', default=None,
                            help='Place to store the new FrameNet frame data')
        parser.add_argument('--relations', default=['ALL'], nargs='+', choices=list(relations_map.keys()),
                            help='Place to store the new FrameNet frame data')
        parser.add_argument('--single_sentences', default=False, action='store_true',
                            help='Forces annotation augmentations to overwrite the '
                                 'same sentence instead of creating new ones')
        parser.add_argument('--num_cpus', default=1, type=int,
                            help='Number of workers to be run in parallel')
        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        assert args.num_cpus > 0, "--num_cpus must be a positive integer"
        config_logger(args)
        if args.save_framenet_path is None and len(args.doc_paths) == 0:
            logger.info("Nothing to be done.")
            exit(0)
        # if args.save_framenet_path is None:
        #     raise NotImplementedError("Saving the frames was not implemented yet")
        return args


    @timeit
    def main(argv):
        args = parse_args(argv)

        parser = NetXMLParser()
        net = parser.parse(args.framenet_path)

        doc_adapter = PARSERS_AVAILABLE[args.document_adapter]()
        relations = tuple(chain(*[relations_map[rel_name] for rel_name in args.relations]))
        ignored_relations = tuple([rel_name for rel_name in StandardAugmentation.ALL_RELATIONS
                                   if rel_name not in relations])
        aug_builder = AugmentationFactory()
        aug_names = list(chain(args.augmentation_types))
        num_cpus = args.num_cpus
        split_in_sentences = not args.single_sentences

        if args.save_framenet_path is not None:
            raise NotImplementedError()
            # for augmentation, aug_name in zip(augmentations, aug_names):
            #     net_aug = augmentation.augment_framenet(net, ignored_relations=ignored_relations)
            #     logger.info(net_aug)
            #     # TODO store the new data

        if len(args.doc_paths) > 0:
            in_path, out_path = args.doc_paths
            doc_list, doc_names = get_docs_and_names(adapter=doc_adapter,
                                                     # file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'dev'))
                                                     file_folder_path=in_path)
            if len(aug_names) == 1:
                if num_cpus > 1:
                    augmentation = ParallelAugmentation(num_cpus, aug_names[0])
                else:
                    augmentation = aug_builder.get_augmentation(aug_names[0])
                docs_aug = augmentation.augment_documents(docs=doc_list, fn=net,
                                                          ignored_relations=ignored_relations,
                                                          split_in_sentences=split_in_sentences)
                for doc, name in zip(docs_aug, doc_names):
                    ensure_dir(out_path)
                    logger.info("Writing '{}'".format(name))
                    doc_adapter.write_doc(doc, path.join(out_path, name))
                # write the file or file list
            if len(aug_names) > 1:
                for aug_name in aug_names:
                    if num_cpus > 1:
                        augmentation = ParallelAugmentation(num_cpus, aug_name)
                    else:
                        augmentation = aug_builder.get_augmentation(aug_name)
                    augmentation_folder = path.join(out_path, "augmentation_{}".format(aug_name))
                    docs_aug = augmentation.augment_documents(docs=doc_list, fn=net,
                                                              ignored_relations=ignored_relations,
                                                              split_in_sentences=split_in_sentences)
                    for doc, name in zip(docs_aug, doc_names):
                        ensure_dir(augmentation_folder)
                        doc_adapter.write_doc(doc, path.join(augmentation_folder, name))
        logger.info("Done")


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
        raise e
    except Exception as e:
        logger.exception(e)
        raise e
