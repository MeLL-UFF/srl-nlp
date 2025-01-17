import logging
from copy import deepcopy as copy
from os import path

import numpy as np
from matplotlib import pyplot as plt
from typing import Dict, List, Tuple

from srl_nlp.framenet import framenet
from srl_nlp.framenet.adapter import FNXMLAdapter, DocumentAdapter
from srl_nlp.framenet.corpus import Document
from srl_nlp.rule_utils import list_doc_files

plt.switch_backend('agg')
logger = logging.getLogger(__name__)


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
            self._update_dict(self._examples, examples)
        self.num_docs = len(docs)

    @staticmethod
    def _update_dict(d1, d2):
        d1.update({k: v for k, v in d2.items() if k not in d1})
        for k, v in d2.items():
            if k in d1:
                if isinstance(v, dict):
                    DocExamplesMap._update_dict(d1[k], v)
                else:
                    d1[k] = d1[k] + v

    @staticmethod
    def get_examples_from_doc(doc, keep_examples):
        # type: (Document, bool) -> Dict[str, Dict[str, object]]
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
                                    frame_dict[frame_element_name] = frame_dict.get(frame_element_name, 0) + 1
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


def hist_ex_by_frame_docs(top, ex_map, color='green', save_path=None):
    # type: (int, DocExamplesMap, str, str) -> None
    """
    Plots histogram of number of examples per per Frame in the Documents
    Args:
        top: Max number of Frames to show
        ex_map:
        color:
        save_path:

    Returns:
        None. It just plots in the plotting device

    """
    frames, nums = zip(*sorted(ex_map.frame_examples.items(),
                               key=lambda x: x[1],
                               reverse=True))
    y_pos = list(range(len(frames)))

    fig = plt.figure(figsize=(10, 4))
    axis1 = fig.add_subplot(1, 2, 1)
    axis1.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color=color)
    axis1.set_yticks(y_pos[:top], frames[:top])
    axis1.set_xlabel('Counting of Examples')
    axis1.set_title('Top {} Frames in Documents'.format(top))

    axis2 = fig.add_subplot(1, 2, 2)
    axis2.barh(y_pos, nums, align='center', alpha=0.5, color=color, height=1.0)
    axis2.set_yticks([])
    axis2.set_ylabel('All {} Frames'.format(len(frames)))
    axis2.set_xlabel('Counting of Examples')
    axis2.set_title('All Frames present in the {} Documents'.format(ex_map.num_docs))

    plt.tight_layout()
    if save_path is None:
        fig.show()
    else:
        fig.savefig(save_path, bbox_inches='tight')


def hist_ex_by_fes_docs(top, ex_map, color='green', save_path=None):
    # type: (int, DocExamplesMap, str, str) -> None
    """
    Plots histogram of number of examples per Frame Element in the Documents
    Args:
        top: Max number of Frames to show
        ex_map:
        color:
        save_path:

    Returns:
        None. It just plots in the plotting device

    """
    fes, nums = zip(*sorted(ex_map.fe_names_examples.items(),
                            key=lambda x: x[1],
                            reverse=True))
    y_pos = list(range(len(fes)))

    fig = plt.figure(figsize=(10, 4))
    axis1 = fig.add_subplot(1, 2, 1)
    axis1.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color=color)
    axis1.set_yticks(y_pos[:top], fes[:top])
    axis1.set_xlabel('Counting of Examples')
    axis1.set_title('Top {} Frame Elements names in Documents'.format(top))

    axis2 = fig.add_subplot(1, 2, 2)
    axis2.barh(y_pos, nums, align='center', alpha=0.5, color=color, height=1.0)
    axis2.set_yticks([])
    axis2.set_ylabel('All {} Frame Elements names'.format(len(fes)))
    axis2.set_xlabel('Counting of Examples')
    axis2.set_title('All Frame Elements names present in the {} Documents'.format(ex_map.num_docs))

    plt.tight_layout()
    if save_path is None:
        fig.show()
    else:
        fig.savefig(save_path, bbox_inches='tight')


def pie_frame_coverage_examples(ex_map, fn, threshold=(1, 2), labels=None, show=True, save_path=None):
    # type: (DocExamplesMap, framenet.FrameNet, List[float], List[str], bool,str) -> Tuple[List[int], List[str]]
    """
    Plots a Pie chart showing the coverage of Frame of FrameNet by Document examples
    Args:
        ex_map: Example map instance with information from the documents to be inspected
        fn: FrameNet instance to be contrasted against
        threshold: list of thresholds for the pie chart
        labels: list of labels, must be of size threshold + 2
        show: show plot
        save_path:

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
        intermediary_labels = [r'${} <$ number of examples $\leq {}$'.format(i, j) if j - i > 1
                               else r'${} i example{}'.format(i, 's' if i > 1 else '')
                               for i, j in ranges]

        labels = [r'less than {} examples'.format(threshold[0])
                  if threshold[0] > 1 else 'No examples'] + intermediary_labels + \
                 [r'more than {} examples'.format(threshold[-1])]
    explode = [0.1] + [0.] * (len(labels) - 1)

    fig, axis = plt.subplots()
    patches, _, _ = axis.pie(sizes, explode=explode,  # labels=labels,
                             autopct='%1.1f%%', pctdistance=.9,
                             counterclock=False, startangle=90)
    axis.set_title("Percentage of FrameNet frame coverage \n by the documents")
    axis.legend(patches, labels)
    axis.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    if show:
        if save_path is None:
            fig.show()
        else:
            fig.savefig(save_path, bbox_inches='tight')
    return sizes, labels


def pie_fe_coverage_examples(ex_map, fn, threshold=(1, 2), labels=None, show=True, save_path=None):
    # type: (DocExamplesMap, framenet.FrameNet, List[float], List[str], bool, str) -> Tuple[List[int], List[str]]
    """
    Plots a Pie chart showing the coverage of Frame Elements of FrameNet by Document examples
    Args:
        ex_map: Example map instance with information from the documents to be inspected
        fn: FrameNet instance to be contrasted against
        threshold: list of thresholds for the pie chart
        labels: list of labels, must be of size threshold + 2
        show: show plot
        save_path:

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
        intermediary_labels = [r'${} <$ number of examples $\leq {}$'.format(i, j) if j - i > 1
                               else r'${} i example{}'.format(i, 's' if i > 1 else '')
                               for i, j in ranges]

        labels = [r'less than {} examples'.format(threshold[0])
                  if threshold[0] > 1 else 'No examples'] + intermediary_labels + \
                 [r'more than {} examples'.format(threshold[-1])]
    explode = [0.1] + [0.] * (len(labels) - 1)

    fig, axis = plt.subplots()
    patches, _, _ = axis.pie(sizes, explode=explode,  # labels=labels,
                             autopct='%1.1f%%', pctdistance=.6,
                             counterclock=False, startangle=90)
    axis.set_title("Percentage of FrameNet Frame Element coverage \n by the documents")
    axis.legend(patches, labels)
    axis.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    if show:
        if save_path is None:
            fig.show()
        else:
            fig.savefig(save_path, bbox_inches='tight')
    return sizes, labels


if __name__ == '__main__':
    import argparse
    from srl_nlp.logger_config import add_logger_args as _add_logger_args, config_logger
    from srl_nlp.framenet.parse_xml import NetXMLParser
    from sys import argv
    from srl_nlp.rule_utils import ensure_dir


    def parse_args(argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Generates some graphs')
        parser.add_argument('report_folder',
                            help='path where to write the reports')
        parser.add_argument('--train', default=None,
                            help='path to the train documents')
        parser.add_argument('--test', default=None,
                            help='path to the test documents')

        parser.add_argument('--dev', default=None,
                            help='path to the dev documents')
        parser.add_argument('--fn_folder', default=None,
                            help='path to the FrameNet folder')
        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        assert args.fn_folder is not None
        return args


    def main():
        args = parse_args(argv, _add_logger_args)
        config_logger(args)

        logger.info(str(args))
        logger.info('Initialization')

        # Select the appropriate document parsers and folder locations
        fn_path = args.fn_folder
        train_docs_path = args.train
        dev_docs_path = args.dev
        test_docs_path = args.test
        report_folder = args.report_folder

        parser = NetXMLParser()
        net = parser.parse(fn_path)
        # hist_frame_by_fe(20, fn=net)
        # hist_ex_by_fe_by_frame(top=20, fn=net)
        # f_fe_2_ex = {for frame in fn.frames.values() for fe in frame.}
        doc_adapter = FNXMLAdapter()

        # doc_list = get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'train')) + \
        #        get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'dev')) + \
        #        get_docs(adapter=adapter, file_folder_path=path.join(DEFAULT_DOCS_ROOT_PATH, 'test'))

        train_doc_list = get_docs(adapter=doc_adapter,
                                  file_folder_path=path.join(train_docs_path)) if train_docs_path else []
        dev_doc_list = get_docs(adapter=doc_adapter, file_folder_path=path.join(dev_docs_path)) if dev_docs_path else []
        test_doc_list = get_docs(adapter=doc_adapter,
                                 file_folder_path=path.join(test_docs_path)) if test_docs_path else []

        doc_list = train_doc_list + dev_doc_list + test_doc_list

        logger.info("Making plots")
        ensure_dir(report_folder)

        examples_map = DocExamplesMap(doc_list, False)
        hist_ex_by_frame_docs(top=20, ex_map=examples_map, save_path=path.join(report_folder, 'anno_by_frame.png'))
        hist_ex_by_fes_docs(top=20, ex_map=examples_map, save_path=path.join(report_folder, 'anno_by_fe.png'))
        pie_frame_coverage_examples(ex_map=examples_map, fn=net, threshold=[1],
                                    save_path=path.join(report_folder, 'coverage1.png'))
        pie_frame_coverage_examples(ex_map=examples_map, fn=net, threshold=[1, 2],
                                    save_path=path.join(report_folder, 'coverage2.png'))
        pie_frame_coverage_examples(ex_map=examples_map, fn=net, threshold=[1, 3],
                                    save_path=path.join(report_folder, 'coverage3.png'))

        logger.info("Writing report")
        with open(path.join(report_folder, "report.txt"), 'w') as report:
            diff_fn_ex = net.frame_names.difference(examples_map.frame_names)
            report.write("There are {} frames in the FrameNet "
                         "that are not represented in the documents\n\n".format(len(diff_fn_ex)))
            diff_ex_fn = examples_map.frame_names.difference(net.frame_names)
            report.write("There are {} frames in the documents that are not present in the FrameNet\n"
                         "They are: {}\n".format(len(diff_ex_fn), ", ".join(map(str, diff_ex_fn))))
        return


    try:
        main()
    except KeyboardInterrupt:
        logger.info('Halted by the user')
        exit(1)
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
