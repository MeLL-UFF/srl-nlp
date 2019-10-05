from __future__ import division

from matplotlib import pyplot as plt
from os import path

from srl_nlp.framenet.parse_xml import NetXMLParser

DEFAULT_VERSION = '1.5'
DEFAULT_FN_ROOT_PATH = '../srl_nlp/framenet/'
DEFAULT_DOCS_ROOT_PATH = '/Users/brenow/DataSets/framenet_docs/paper'


def get_fn(fn_version=DEFAULT_VERSION, fn_root_path=DEFAULT_FN_ROOT_PATH):
    parser = NetXMLParser()
    return parser.parse(path.join(fn_root_path, 'fndata-{}'.format(fn_version)))


def hist_frame_by_fe(top, fn_version=DEFAULT_VERSION, fn_root_path=DEFAULT_FN_ROOT_PATH, fn=None, color='blue',
                     alpha=0.5):
    if fn is None:
        fn = get_fn(fn_version=fn_version, fn_root_path=fn_root_path)

    fes, nums = zip(*sorted([(fe, len(fn.get_frame_element_frames(fe))) for fe in fn.fe_names],
                            key=lambda x: x[1],
                            reverse=True))
    y_pos = list(range(len(fes)))
    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=alpha, color=color)
    plt.yticks(y_pos[:top], fes[:top])
    plt.xlabel('Counting of Frame appearences')
    plt.title('Top {top} Frame Elements in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=alpha, color=color)
    plt.yticks([y for y in y_pos if y % 10 == 0])
    # plt.ylabel('All {} Frame Elements'.format(len(fes)))
    # plt.hlines([i * max(y_pos) for i in np.arange(.1, 1.01, .25)], 0, max(nums), linestyles='dashed')
    plt.xlabel('Counting of Frame appearences')
    plt.title('All Frame Elements in FrameNet {ver}'.format(top=top, ver=fn_version))
    plt.yticks([])

    # plt.tight_layout()
    plt.show()


def hist_ex_by_frame(top, fn_version=DEFAULT_VERSION, fn_root_path=DEFAULT_FN_ROOT_PATH, fn=None):
    """
    Plots histogram of number of examples per Frame in the FrameNet structure
    Args:
        top: Max number of Frames to show
        fn_version: FrameNet version to be used
        fn_root_path: Path to the folder storing the FrameNet folder
        fn: Optional, you can use a pre-loaded FrameNet

    Returns:
        None. It just plots in the plotting device

    """
    if fn is None:
        fn = get_fn(fn_version=fn_version, fn_root_path=fn_root_path)

    def get_examples(f):
        fes = f.coreFEs + f.peripheralFEs
        examples = []
        for fe in fes:
            examples.extend(fe.definition.get_elements('ex'))
        return examples

    frames, nums = zip(*sorted([(frame.name, len(get_examples(frame))) for frame in fn],
                               key=lambda x: x[1],
                               reverse=True))
    y_pos = list(range(len(frames)))

    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color='green')
    plt.yticks(y_pos[:top], frames[:top])
    plt.xlabel('Counting of Examples')
    plt.title('Top {top} Frames in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=0.5, color='green')
    plt.yticks([])
    plt.ylabel('All {} Frames'.format(len(frames)))
    plt.xlabel('Counting of Examples')
    plt.title('All Frames in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.tight_layout()
    plt.show()


def hist_ex_by_fe_by_frame(top, fn_version=DEFAULT_VERSION, fn_root_path=DEFAULT_FN_ROOT_PATH, fn=None):
    """
    Plots histogram of number of examples / total of Frame Elements per Frame in the FrameNet structure
    Args:
        top: Max number of Frames to show
        fn_version: FrameNet version to be used
        fn_root_path: Path to the folder storing the FrameNet folder
        fn: Optional, you can use a pre-loaded FrameNet

    Returns:
        None. It just plots in the plotting device

    """
    if fn is None:
        fn = get_fn(fn_version=fn_version, fn_root_path=fn_root_path)

    def get_num_examples(f):
        fes = f.coreFEs + f.peripheralFEs
        examples = []
        for fe in fes:
            examples.extend(fe.definition.get_elements('ex'))
            # print len(fe.definition.get_elements('ex'))
        # assert len(examples) != (len(f.coreFEs) + len(f.peripheralFEs))
        return len(examples) / (len(f.coreFEs) + len(f.peripheralFEs))

    frames, nums = zip(*sorted([(frame.name, get_num_examples(frame)) for frame in fn],
                               key=lambda x: x[1],
                               reverse=True))
    y_pos = list(range(len(frames)))

    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color='mediumseagreen')
    plt.yticks(y_pos[:top], frames[:top])
    plt.xlabel(r'Counting of $\frac{Examples}{Frame Elements}$')
    plt.title('Top {top} Frames in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=0.5, color='mediumseagreen')
    # plt.yticks(y_pos, frames)
    plt.yticks([])
    plt.ylabel('All {} Frames'.format(len(frames)))
    plt.xlabel(r'Counting of $\frac{Examples}{Frame Elements}$')
    plt.title('All Frames in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.tight_layout()
    plt.show()


def hist_ex_by_fe(top, fn_version=DEFAULT_VERSION, fn_root_path=DEFAULT_FN_ROOT_PATH, fn=None):
    """
    Plots histogram of number of examples per Frame Element name in the FrameNet structure
    Args:
        top: Max number of Frames to show
        fn_version: FrameNet version to be used
        fn_root_path: Path to the folder storing the FrameNet folder
        fn: Optional, you can use a pre-loaded FrameNet

    Returns:
        None. It just plots in the plotting device

    """
    if fn is None:
        fn = get_fn(fn_version=fn_version, fn_root_path=fn_root_path)

    def get_examples(frame_element_name, framenet):
        frame_element = framenet.get_frame_element(frame_element_name)
        frame_list = framenet.get_frame_element_frames(frame_element)
        examples = []
        for f in frame_list:
            fe_list = f.coreFEs + f.peripheralFEs

            for other_fe in fe_list:
                if other_fe.name == frame_element.name:
                    examples.extend(other_fe.definition.get_elements('ex'))
        return examples

    fes, nums = zip(*sorted([(fe, len(get_examples(fe, fn))) for fe in fn.fe_names],
                            key=lambda x: x[1],
                            reverse=True))

    y_pos = list(range(len(fes)))

    plt.figure(figsize=(10, 4))
    plt.subplot(1, 2, 1)
    plt.barh(y_pos[:top], nums[:top], align='center', alpha=0.5, color='purple')
    plt.yticks(y_pos[:top], fes[:top])
    plt.xlabel('Counting of Examples')
    plt.title('Top {top} Frame Elements in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.subplot(1, 2, 2)
    plt.barh(y_pos, nums, align='center', alpha=0.5, color='purple')
    plt.yticks([])
    plt.ylabel('All {} Frame Elements'.format(len(fes)))
    plt.xlabel('Counting of Examples')
    plt.title('All Frame Elements in FrameNet {ver}'.format(top=top, ver=fn_version))

    plt.tight_layout()
    plt.show()


if __name__ == '__main__':
    net = get_fn()
    top_val = 20
    hist_frame_by_fe(top_val, fn=net)
    hist_ex_by_frame(top_val, fn=net)
    hist_ex_by_fe_by_frame(top=top_val, fn=net)
    hist_ex_by_fe(top=top_val, fn=net)
