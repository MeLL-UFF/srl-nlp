#!/bin/env python

"""
Parses sesame analysis
"""
import logging
import pandas as pd
import re
from functools import reduce
from matplotlib import pyplot as plt
from matplotlib.axes import Axes
from os import path, listdir

from srl_nlp.framenet.description import EXample
from srl_nlp.framenet.parse_xml import NetXMLParser

logger = logging.getLogger(__name__)

# text = """Sent#1 :
# tokens and depparse:
# your contribution to goodwill will mean more than you may know .
# gold:
# frame:GOAL
# Trajector	your contribution
# Landmark	goodwill
#
# prediction:Trajector	your contribution
# Landmark	goodwill
#
# 2.0 / 2.0 / 2.0
# gold:
# frame:GIVING
# Recipient	to goodwill
# Donor	your
#
# prediction:Recipient	to goodwill
# Donor	your
#
# 2.0 / 2.0 / 2.0
# gold:
# frame:AWARENESS
# Cognizer	you
#
# prediction:Cognizer	more than you
#
# 0.0 / 1.0 / 1.0
# gold:
# frame:PURPOSE
# Means	your contribution to goodwill
# Value	more than you may know
#
# prediction:Agent	your contribution to goodwill
# Goal	more than you may know
#
# 0.0 / 2.0 / 2.0
# gold:
# frame:INCREMENT
# Class	than you may know
#
# prediction:Class	than you
#
# 0.0 / 1.0 / 1.0
# gold:
# frame:LIKELIHOOD
# Hypothetical_event	you
# Hypothetical_event	know
#
# prediction:Hypothetical_event	more than you
# Hypothetical_event	know
#
# 0.0 / 1.0 / 1.0
# 									Total: 4.0 / 9.0 / 9.0
# """


sent_pattern = re.compile(r"""Sent#(\d+) :
tokens and depparse:
.*
(gold:
frame:(?:.*)
[\s\S]*?
prediction:[\s\S]*?

(?:\d*\.?\d+) / (?:\d*\.?\d+) / (?:\d*\.?\d))+
\t+Total: (?:\d*\.?\d+) / (?:\d*\.?\d+) / (?:\d*\.?\d)""")

f_pattern = re.compile(r"""gold:
frame:(\w*)
([\s\S]*?)

prediction:([\s\S]*?)

(\d*\.?\d+) / (\d*\.?\d+) / (\d*\.?\d)""")


def parse_analysis(text, suffix=''):
    # type: (str, str) -> pd.DataFrame
    sentences = sent_pattern.findall(text)
    sids, frames, tps, fns, fps = [], [], [], [], []
    for sentence in sentences:
        sid = sentence[0]
        scores = f_pattern.findall(sentence[1])
        for score in scores:
            frame, tp, fn, fp = map(lambda x: score[x], [0, 3, 4, 5])
            logger.debug(",".join([sid, frame, tp, fn, fp]))
            sids.append(int(sid))
            frames.append(frame)
            tps.append(float(tp))
            fns.append(float(fn))
            fps.append(float(fp))
    df = pd.DataFrame({
        'sid': sids,
        'frame': frames,
        'tp{}'.format(suffix): tps,
        'fn{}'.format(suffix): fns,
        'fp{}'.format(suffix): fps
    })
    return df


def frame_example_count(fn, foo=lambda x: x):
    def count_ex(frame):
        count = sum([len(fe.definition.get_elements(EXample)) for fe in frame.coreFEs + frame.peripheralFEs])
        # count = count + len(frame.get_elements(EXample))
        return count

    return {foo(frame.name): count_ex(frame) for frame in fn}


def plot_frame_example_hist(df, title="Histogram of examples per frame", bins=30, save_fig=None, show_plot=True):
    frame_count = df[["frame", "frame_example_count"]]  # type: pd.DataFrame
    frame_count.drop_duplicates(inplace=True)
    axis = frame_count.iloc[:, 1].hist(grid=False, bins=bins)  # type: Axes
    axis.set_title(title)
    axis.set_ylabel("Frequency")
    axis.set_xlabel("Examples per Frame")
    fig = axis.figure
    if save_fig is not None:
        fig.savefig(save_fig)
    if show_plot:
        plt.show()
    plt.close(fig)


def plot_fn_frame_example_hist(fn_dict, title="Histogram of examples per frame", bins=30, save_fig=None,
                               show_plot=True):
    frame_count = list(fn_dict.values())
    fig, axis = plt.subplots()
    axis.hist(frame_count, bins=bins)
    axis.set_title(title)
    axis.set_ylabel("Frequency")
    axis.set_xlabel("Examples per Frame")
    if save_fig is not None:
        fig.savefig(save_fig)
    if show_plot:
        plt.show()
    plt.close(fig)


if __name__ == '__main__':
    import argparse
    from sys import argv
    from srl_nlp.logger_config import add_logger_args, config_logger


    def parse_args():
        parser = argparse.ArgumentParser(description='Processes sesame output')
        parser.add_argument('root_path', help='path where to write the reports')
        parser.add_argument('--framenet_path', default='srl_nlp/framenet/fndata-1.5', help='Path to FrameNet Folder')
        parser.add_argument('--out_df', default=None, help='Path to store the generated df')

        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        return args


    def main():
        args = parse_args()
        config_logger(args)
        exp_path = args.root_path
        dfs = []
        logger.info("Reading from '{}'".format(exp_path))
        for d_id, au_name in enumerate(sorted(listdir(exp_path))):
            au_path = path.join(exp_path, au_name)
            if path.isdir(au_path) and au_name != 'test':
                full_path = path.join(au_path, 'methods', 'sesame', 'logs', 'argid', 'argid-prediction-analysis.log')
                if path.isfile(full_path):
                    with open(full_path, 'r') as f:
                        text = f.read()
                        df = parse_analysis(text, '_{}'.format(au_name))
                        df = df.groupby(by=['frame', 'sid'], as_index=False) \
                            .sum() \
                            .sort_values(['sid', 'frame'])
                        dfs.append(df)
        df = reduce(lambda x, y: pd.merge(x, y, on=['frame', 'sid']), dfs)  # type: pd.DataFrame
        logger.info("Parsing framenet from '{}'".format(args.framenet_path))
        parser = NetXMLParser()
        fn = parser.parse(args.framenet_path)
        frame_ex_count = frame_example_count(fn, str.upper)
        df['frame_example_count'] = df['frame'].map(frame_ex_count)

        if args.out_df is not None:
            logger.info("Storing df as '{}'".format(args.out_df))
            df.to_pickle(args.out_df)


    try:
        main()
    except KeyboardInterrupt:
        logger.error('Halted by the user')
        exit(1)
    except OSError as e:
        logger.error('Problem reading/writing files')
        logger.error(e)
        raise e
