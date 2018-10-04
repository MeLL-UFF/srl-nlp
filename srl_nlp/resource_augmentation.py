import logging
from typing import Tuple, List, Union, Dict

from notebooks.fn_graphs import get_fn
from srl_nlp.framenet import framenet
from srl_nlp.framenet.corpus import Document
from copy import deepcopy as copy

from srl_nlp.framenet.framenet import Description

FE = framenet.Frame.Element
logger = logging.getLogger(__name__)


class StandardAugmentation(object):

    def __init__(self):
        self.ex_count = 0

    def augment(self, fn, docs=None):
        # type: (framenet.Net, List[Document]) -> Tuple[framenet.Net, List[Document]]
        self.ex_count = 0
        new_frames = [self.augment_frame(frame, fn) for frame in fn]
        fn_augmented = framenet.Net(new_frames)
        logger.info('There were added {} examples'.format(self.ex_count))
        if docs is not None:
            docs = [self.augment_document(doc) for doc in docs]
        else:
            docs = []
        return fn_augmented, docs

    def augment_frame(self, frame_or_frame_name, fn, ignored_relations=('See also',)):
        # type: (Union[framenet.Frame, str], framenet.Net, Tuple[str]) -> object
        if isinstance(frame_or_frame_name, str):
            frame = fn[frame_or_frame_name]
        else:
            frame = frame_or_frame_name

        neighbour_frames = [neighbour_frame for name, relation in frame.relations.items()
                            for neighbour_frame in relation
                            if name not in ignored_relations]
        core_fes, peripheral_fes = self._augment_frame_aux(frame.coreFEs, frame.peripheralFEs, neighbour_frames)

        return framenet.Frame(name=frame.name, description=frame.description, idx=frame.id, lus=frame.LUs,
                              core_fes=core_fes, peripheral_fes=peripheral_fes, **frame.relations)

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
                            self.ex_count = self.ex_count +1
                        except KeyError as e:
                            logger.warning('Missing fe {}'.format(e))
        return core_fes, peripheral_fes

    def _get_fe_dict(self, fe_list_old, fe_list_new):
        # type: (List[FE], List[FE]) -> Dict[FE,FE]
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
        return False  # TODO

    @staticmethod
    def _convert_fn_example(new_example, dict_fes, invert_dict=False):
        # type: (Description.EXample, Dict[FE,FE], bool) -> Description.EXample
        if invert_dict:
            dict_fes = {v: k for k, v in dict_fes.items()}  # type: Dict[FE,FE]
        name_abbrev2fe = {fe_from.name: fe_to for fe_from, fe_to in dict_fes.items()}  # type: Dict[str,FE]
        name_abbrev2fe.update({fe_from.abbrev: fe_to for fe_from, fe_to in dict_fes.items()})

        converted_example = copy(new_example)
        for element in converted_example:
            if isinstance(element, Description.FEeXample):
                fex = element  # type: Description.FEeXample
                fex.attribs['name'] = name_abbrev2fe[fex.attribs['name']].abbrev
        return converted_example

    def augment_document(self, doc):
        return doc  # TODO


class SyntaticAugmentation(StandardAugmentation):
    pass


if __name__ == '__main__':  # TODO remove that : do proper testing later
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s")
    net = get_fn()
    augmentation = StandardAugmentation()
    net_augmented = augmentation.augment(net)
    pass
