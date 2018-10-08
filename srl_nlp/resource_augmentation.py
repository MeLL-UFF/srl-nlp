from os import path

import logging
from typing import Tuple, List, Union, Dict

from srl_nlp.framenet import framenet
from srl_nlp.framenet.corpus import Document, Paragraph, Sentence, AnnotationSet
from copy import deepcopy as copy

from srl_nlp.framenet.framenet import Description

FE = framenet.Frame.Element
logger = logging.getLogger(__name__)


class StandardAugmentation(object):

    def __init__(self):
        self.ex_count = 0
        self.doc_ex_count = 0
        self.total_sentences = 0

    def augment(self, fn, docs=None, augment_fn=True):
        # type: (framenet.Net, List[Document], bool) -> Tuple[framenet.Net, List[Document]]
        self.ex_count = 0
        self.doc_ex_count = 0
        self.total_sentences = 0
        new_frames = [self.augment_frame(frame, fn) for frame in fn]
        if augment_fn:
            fn_augmented = framenet.Net(new_frames)
            logger.info('There were added {} fn examples'.format(self.ex_count))
        else:
            logger.info('Skipping FrameNet augmentation')
            fn_augmented = fn
        if docs is not None:
            logger.info('Augmenting documents')
            docs = [self.augment_document(doc, fn) for doc in docs]
            logger.info('There were added {} doc sentence examples'.format(self.doc_ex_count))
            logger.info('There were {} doc sentence examples'.format(self.total_sentences))
        else:
            docs = []
        return fn_augmented, docs

    def augment_frame(self, frame_or_frame_name, fn, ignored_relations=('See also',)):
        # type: (Union[framenet.Frame, str], framenet.Net, Tuple[str]) -> object
        if isinstance(frame_or_frame_name, str):
            frame = fn[frame_or_frame_name]
        else:
            frame = frame_or_frame_name

        neighbour_frames = self.get_neighbours(frame, ignored_relations)
        core_fes, peripheral_fes = self._augment_frame_aux(frame.coreFEs, frame.peripheralFEs, neighbour_frames)

        return framenet.Frame(name=frame.name, description=frame.description, idx=frame.id, lus=frame.LUs,
                              core_fes=core_fes, peripheral_fes=peripheral_fes, **frame.relations)

    @staticmethod
    def get_neighbours(frame, ignored_relations):
        # type: (framenet.Frame, Tuple[str]) -> List[framenet.Frame]
        neighbour_frames = [neighbour_frame for name, relation in frame.relations.items()
                            for neighbour_frame in relation
                            if name not in ignored_relations]
        return neighbour_frames

    def _augment_frame_aux(self, core_fes, peripheral_fes, neighbour_frames):
        # type: (List[FE], List[FE], List[framenet.Frame]) -> Tuple[List[FE], List[FE]]

        core_fes = copy(core_fes)
        peripheral_fes = copy(peripheral_fes)

        for neighbour_frame in neighbour_frames:
            dict_fes = self._get_fe_dict(core_fes + peripheral_fes, neighbour_frame.fes)
            for fe in core_fes:
                if fe in dict_fes:
                    for new_example in dict_fes[fe].definition.get_elements(Description.EXample):
                        try:
                            converted_example = self._convert_fn_example(new_example, dict_fes, invert_dict=True)
                            fe.definition.add_element(converted_example)
                            self.ex_count = self.ex_count + 1
                        except KeyError as e:
                            logger.warning('Missing fe {}'.format(e))
        return core_fes, peripheral_fes

    def _get_fe_dict(self, fe_list_old, fe_list_new):
        # type: (List[FE], List[FE]) -> Dict[FE,FE]
        """

        Args:
            fe_list_old: list of frame elements to use as key of the dictionary
            fe_list_new: list of frame elements to use as value of the dictionary

        Returns:
            A dictionary that matches frame elements from a list to their related frame elements in the other list
        """
        dict_fes = {fe: None for fe in fe_list_old}
        for other_fe in fe_list_new:  # Test for name equality
            for fe in dict_fes:
                if dict_fes[fe] is None and fe == other_fe:
                    dict_fes[fe] = other_fe
                    break
        for other_fe in fe_list_new:  # Test for similarity
            for fe in dict_fes:
                if dict_fes[fe] is None and self._fes_are_similiar(fe, other_fe):
                    dict_fes[fe] = other_fe
                    break
        return {k: v for k, v in dict_fes.items() if v is not None}

    @staticmethod
    def _fes_are_similiar(fe, other_fe):
        logger.debug("{} not similar to {}".format(fe, other_fe))
        return False

    @staticmethod
    def _convert_fn_example(example, dict_fes, invert_dict=False):
        # type: (Description.EXample, Dict[FE,FE], bool) -> Description.EXample
        if invert_dict:
            dict_fes = {v: k for k, v in dict_fes.items()}  # type: Dict[FE,FE]
        name_abbrev2fe = {fe_from.name: fe_to for fe_from, fe_to in dict_fes.items()}  # type: Dict[str,FE]
        name_abbrev2fe.update({fe_from.abbrev: fe_to for fe_from, fe_to in dict_fes.items()})

        new_example = copy(example)
        for element in new_example:
            if isinstance(element, Description.FEeXample):
                fex = element  # type: Description.FEeXample
                fex.attribs['name'] = name_abbrev2fe[fex.attribs['name']].abbrev
        return new_example

    def augment_document(self, doc, fn, ignored_relations=('See also',)):
        new_elements = []
        for element in doc:
            if isinstance(element, Paragraph):
                new_paragraph = copy(element)  # type: Paragraph
                for sentence in new_paragraph:
                    self.augment_doc_sentence_inplace(sentence, fn, ignored_relations=ignored_relations)
                new_elements.append(new_paragraph)
            elif isinstance(element, Sentence):
                new_sentence = copy(element)  # type: Sentence
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
        # type: (Sentence, framenet.Net, Tuple[str]) -> None
        new_anno_sets = []
        for annotation_set in sentence:
            frame = fn[annotation_set.frameName]
            neighbour_frames = self.get_neighbours(frame, ignored_relations)
            fes = [fn.get_frame_element(layer.name)
                   for layer in annotation_set.get_fes()
                   if fn.has_frame_element(layer.name)]
            # TODO decide if I drop the augmentation if there are fes not in the framenet
            self.total_sentences = self.total_sentences + 1
            for neighbour_frame in neighbour_frames:
                fes_dict = self._get_fe_dict(fes, neighbour_frame.fes)
                if len(fes_dict) > 0 and len(fes_dict) == len(fes):
                    # if this condition is met, then we can make the transference:
                    self.doc_ex_count = self.doc_ex_count + 1
                    new_anno_sets.append(self.augment_doc_anno_set(annotation_set,
                                                                   neighbour_frame.name,
                                                                   neighbour_frame.id,
                                                                   fes_dict))
        sentence.annotation_sets.append(new_anno_sets)
        return None

    def augment_doc_anno_set(self, annotation_set, frame_name, frame_idx, fes_dict):
        # type: (AnnotationSet, str, Union[int, str], Dict[FE,FE]) -> AnnotationSet
        str2fe = {fe_from.name: fe_to for fe_from, fe_to in fes_dict.items()}
        new_annotation_set = copy(annotation_set)
        new_annotation_set.frameName = frame_name
        new_annotation_set.frameID = frame_idx
        for layer in new_annotation_set:
            if layer.name.lower() == 'fe':
                for annotation in layer:
                    annotation.name = str2fe[annotation.name]
        return new_annotation_set


class SyntaticAugmentation(StandardAugmentation):

    @staticmethod
    def _fes_are_similiar(fe, other_fe):
        logger.debug("{} not similar to {}".format(fe, other_fe))
        return False  # TODO


if __name__ == '__main__':  # TODO remove that : do proper testing later
    from notebooks.doc_graphs import get_docs, DEFAULT_DOCS_ROOT_PATH
    from notebooks.fn_graphs import get_fn
    from srl_nlp.framenet.adapter import FNXMLAdapter

    logging.basicConfig(level=logging.INFO, format="[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s")
    net = get_fn()
    doc_adapter = FNXMLAdapter()
    doc_list = get_docs(adapter=doc_adapter,
                        file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'train', 'ANC__110CYL068.xml'))
    augmentation = StandardAugmentation()
    net_augmented = augmentation.augment(net, doc_list)
    logger.info("Done")
    pass
