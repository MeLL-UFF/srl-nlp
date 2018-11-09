from __future__ import print_function, division

import re

from typing import BinaryIO, Tuple


def calc_f(tp, fp, fn):
    if tp == 0.0 and fp == 0.0:
        pr = 0.0
    else:
        pr = tp / (tp + fp)
    if tp == 0.0 and fn == 0.0:
        re = 0.0
    else:
        re = tp / (tp + fn)
    if pr == 0.0 and re == 0.0:
        f = 0.0
    else:
        f = 2.0 * pr * re / (pr + re)
    return pr, re, f


def process_evaluation_file(f_stream):
    # type: (BinaryIO) -> Tuple[float,float,float]
    tp, fp, fn = 0, 0, 0
    line_pattern = re.compile(r"(\d*\.?\d+)\s*/\s*(\d*\.?\d+)\s*/\s*(\d*\.?\d+)")
    ok_flag = False
    for line in f_stream:
        match = line_pattern.match(line)
        if match:
            ok_flag=True
            example_tp = float(match.group(1))
            example_fp = float(match.group(2)) - float(match.group(1))
            example_fn = float(match.group(3)) - float(match.group(1))
            tp = tp + example_tp
            fp = fp + example_fp
            fn = fn + example_fn

    assert ok_flag, "No match found in file"
    p, r, f1 = calc_f(tp, fp, fn)
    return p, r, f1


if __name__ == '__main__':
    from sys import stdin
    import argparse


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
        # args = parse_args(argv, _add_logger_args)
        # config_logger(args)
        # file_path = ''
        # logger.info(str(args))
        #logger.info('Initialization')

        for file_path in stdin:
            #logger.info("Reading file {}".format(file_path))
            with open(file_path.strip(), 'r') as report:
                #print("Precision '{}', Recall: '{}' and F1:'{}'".format(*process_evaluation_file(report)))
                print("{}, {}, {}".format(*process_evaluation_file(report)))


    try:
        main()
    except KeyboardInterrupt:
        print('Halted by the user')
        exit(1)
    except OSError as e:
        print('Problem reading/writing files')
        raise e
