import numpy as np
from copy import deepcopy as copy
from matplotlib import pyplot as plt
from os import path
from typing import Dict, List, Tuple

from notebooks.fn_graphs import get_fn
from srl_nlp.framenet import framenet
from srl_nlp.framenet.adapter import FNXMLAdapter, DocumentAdapter
from srl_nlp.framenet.corpus import Document
from srl_nlp.rule_utils import list_doc_files

DEFAULT_DOCS_ROOT_PATH = '/Users/brenow/DataSets/framenet_docs/paper'


def get_docs(adapter, file_folder_path):
    # type: (DocumentAdapter, str) -> List[Document]
    docs = []  # type: List[Document]
    if path.isfile(file_folder_path):
        docs.extend(adapter.parse_file(file_folder_path))
    else:
        for doc_name in list_doc_files(file_folder_path):
            docs.extend(adapter.parse_file(path.join(file_folder_path, doc_name)))
    return docs


class DocExamplesMap:

    def __init__(self, docs, keep_examples=True):
        self._examples = dict()
        for doc in docs:
            examples = self.get_examples_from_doc(doc, keep_examples)
            for frame_name, fes in examples.items():
                for fe_name, val in fes.items():
                    self._examples[frame_name] = self._examples.get(frame_name, dict())
                    if fe_name in self._examples[frame_name]:
                        self._examples[frame_name][fe_name] = self._examples[frame_name][fe_name] + \
                                                              examples[frame_name][fe_name]
                    else:
                        self._examples[frame_name][fe_name] = copy(examples[frame_name][fe_name])
            self._examples = examples
        self.num_docs = len(docs)

    @staticmethod
    def get_examples_from_doc(doc, keep_examples):
        # type: (Document, bool) -> Dict[Dict[object]]
        examples = dict()
        for paragraph in doc:
            for sentence in paragraph:
                for annotation_set in sentence:  # .annotation_sets:
                    frame_name = annotation_set.frameName
                    frame_dict = examples.get(frame_name, dict())
                    for layer in annotation_set:
                        if layer.name == 'FE':
                            for annotation in layer:
                                frame_element_name = annotation.name
                                if keep_examples:
                                    if frame_element_name not in frame_dict:
                                        frame_dict[frame_element_name] = []
                                    frame_dict[frame_element_name].append(sentence.text)
                                else:
                                    if frame_element_name not in frame_dict:
                                        frame_dict[frame_element_name] = 0
                                    frame_dict[frame_element_name] = frame_dict[frame_element_name] + 1
                    examples[frame_name] = frame_dict
        return examples

    @property
    def frame_names(self):
        return set(self._examples.keys())

    @property
    def fe_names(self):
        return {fe for v, fes in self._examples.items() for fe in fes}

    @property
    def frame_examples(self):
        return {frame: sum(fes.values()) for frame, fes in self._examples.items()}

    @property
    def fe_names_examples(self):
        out = dict()
        for fes in self._examples.values():
            for fe in fes:
                if fe in out:
                    # out[fe].extend(fes[fe])
                    out[fe] = out[fe] + (fes[fe])
                else:
                    out[fe] = copy(fes[fe])
        return out


def hist_ex_by_frame_docs(top, ex_map, color='green'):
    # type: (int, DocExamplesMap, str) -> None
    """
    Plots histogram of number of examples per per Frame in the Documents
    Args:
        top: Max number of Frames to show
        ex_map:
        color:

    Returns:
        None. It just plots in the plotting device

    """
    frames, nums = zip(*sorted(ex_map.frame_examples.items(),
                               key=lambda x: x[1],
                               reverse=True))
    y_pos = list(range(len(frames)))

    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color=color)
    plt.yticks(y_pos[:top], frames[:top])
    plt.xlabel('Counting of Examples')
    plt.title('Top {} Frames in Documents'.format(top))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=0.5, color=color, height=1.0)
    plt.yticks([])
    plt.ylabel('All {} Frames'.format(len(frames)))
    plt.xlabel('Counting of Examples')
    plt.title('All Frames present in the {} Documents'.format(ex_map.num_docs))

    plt.tight_layout()
    plt.show()


def hist_ex_by_fes_docs(top, ex_map, color='green'):
    # type: (int, DocExamplesMap, str) -> None
    """
    Plots histogram of number of examples per Frame Element in the Documents
    Args:
        top: Max number of Frames to show
        ex_map:
        color:

    Returns:
        None. It just plots in the plotting device

    """
    fes, nums = zip(*sorted(ex_map.fe_names_examples.items(),
                            key=lambda x: x[1],
                            reverse=True))
    y_pos = list(range(len(fes)))

    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color=color)
    plt.yticks(y_pos[:top], fes[:top])
    plt.xlabel('Counting of Examples')
    plt.title('Top {} Frame Elements names in Documents'.format(top))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=0.5, color=color, height=1.0)
    plt.yticks([])
    plt.ylabel('All {} Frame Elements names'.format(len(fes)))
    plt.xlabel('Counting of Examples')
    plt.title('All Frame Elements names present in the {} Documents'.format(ex_map.num_docs))

    plt.tight_layout()
    plt.show()


def pie_frame_coverage_examples(ex_map, fn, threshold=(1, 2), labels= None, show=True):
    # type: (DocExamplesMap, framenet.Net, List[float], List[str], bool ) -> Tuple[List[int], List[str]]
    """
    Plots a Pie chart showing the coverage of Frame of FrameNet by Document examples
    Args:
        ex_map: Example map instance with information from the documents to be inspected
        fn: FrameNet instance to be contrasted against
        threshold: list of thresholds for the pie chart
        labels: list of labels, must be of size threshold + 2
        show: show plot

    Returns:
        None. It just plots in the plotting device
    """
    ex_count = {frame: 0 for frame in fn.frame_names}
    ex_count.update((key, ex_map.frame_examples[key]) for key in ex_count.keys() if key in ex_map.frame_examples)
    vals = np.array(ex_count.values())
    ranges = list(zip(threshold[:-1], threshold[1:]))
    sizes = [np.where(vals <= threshold[0])[0].size] + \
            [np.where((i < vals) & (vals <= j))[0].size for i, j in ranges] + \
            [np.where(vals > threshold[-1])[0].size]
    if labels is None:
        labels = [r'num < ${}'.format(threshold[0]) if threshold[0] > 1 else 'No examples'] + \
                 [r'${} < num \leq {}$'.format(i, j) for i, j in ranges] + \
                 [r'${} < num$'.format(threshold[-1])]
    explode = [0.1] + [0.] * (len(labels) - 1)

    patches, _, _ = plt.pie(sizes, explode=explode,  # labels=labels,
                            autopct='%1.1f%%', pctdistance=.9,
                            counterclock=False, startangle=90)
    plt.title("Percentage of FrameNet frame coverage \n by the documents")
    plt.legend(patches, labels)
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    if show:
        plt.show()
    return sizes, labels


def pie_fe_coverage_examples(ex_map, fn, threshold=(1, 2), labels=None, show=True):
    # type: (DocExamplesMap, framenet.Net, List[float], List[str], bool ) -> Tuple[List[int], List[str]]
    """
    Plots a Pie chart showing the coverage of Frame Elements of FrameNet by Document examples
    Args:
        ex_map: Example map instance with information from the documents to be inspected
        fn: FrameNet instance to be contrasted against
        threshold: list of thresholds for the pie chart
        labels: list of labels, must be of size threshold + 2
        show: show plot

    Returns:
        None. It just plots in the plotting device
    """
    ex_count = {frame: 0 for frame in fn.frame_names}
    ex_count.update((key, ex_map.fe_names_examples[key]) for key in ex_count.keys() if key in ex_map.fe_names_examples)
    vals = np.array(ex_count.values())
    ranges = list(zip(threshold[:-1], threshold[1:]))
    sizes = [np.where(vals <= threshold[0])[0].size] + \
            [np.where((i < vals) & (vals <= j))[0].size for i, j in ranges] + \
            [np.where(vals > threshold[-1])[0].size]
    if labels is None:
        labels = [r'num < ${}'.format(threshold[0]) if threshold[0] > 1 else 'No examples'] + \
                 [r'${} < num \leq {}$'.format(i, j) for i, j in ranges] + \
                 [r'${} < num$'.format(threshold[-1])]
    explode = [0.1] + [0.] * (len(labels) - 1)

    patches, _, _ = plt.pie(sizes, explode=explode,  # labels=labels,
                            autopct='%1.1f%%', pctdistance=.6,
                            counterclock=False, startangle=90)
    plt.title("Percentage of FrameNet Frame Element coverage \n by the documents")
    plt.legend(patches, labels)
    plt.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    if show:
        plt.show()
    return sizes, labels


if __name__ == '__main__':
    net = get_fn()
    # hist_frame_by_fe(20, fn=net)
    # hist_ex_by_fe_by_frame(top=20, fn=net)
    # f_fe_2_ex = {for frame in fn.frames.values() for fe in frame.}
    doc_adapter = FNXMLAdapter()

    # doc_list = get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'train')) + \
    #        get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'dev')) + \
    #        get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'test'))

    doc_list = get_docs(adapter=doc_adapter,
                        file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'train', 'ANC__110CYL068.xml'))

    examples_map = DocExamplesMap(doc_list, False)
    hist_ex_by_frame_docs(top=20, ex_map=examples_map)
    hist_ex_by_fes_docs(top=20, ex_map=examples_map)
    pie_frame_coverage_examples(ex_map=examples_map, fn=net)

    diff_fn_ex = net.frame_names.difference(examples_map.frame_names)
    print("There are {} frames in the FrameNet that are not represented in the documents".format(len(diff_fn_ex)))
    diff_ex_fn = examples_map.frame_names.difference(net.frame_names)
    print("There are {} frames in the documents that are "
          "not present in the FrameNet\nThey are: {}".format(len(diff_ex_fn), ", ".join(diff_ex_fn)))
    pass
