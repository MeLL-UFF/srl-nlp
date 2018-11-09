"""
Parses sesame analysis
"""
import pandas as pd
import re
from os import path, listdir

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
            print(",".join([sid, frame, tp, fn, fp]))
            sids.append(int(sid))
            frames.append(frame)
            tps.append(tp)
            fns.append(fn)
            fps.append(fp)
    df = pd.DataFrame({
        'sid': sids,
        'frame': frames,
        'tp{}'.format(suffix): tps,
        'fn{}'.format(suffix): fns,
        'fp{}'.format(suffix): fps
    })
    return df


if __name__ == '__main__':
    import argparse
    from sys import argv


    def parse_args(argv):
        parser = argparse.ArgumentParser(description='Processes sesame output')
        parser.add_argument('root_path',
                            help='path where to write the reports')
        args = parser.parse_args(argv[1:])
        return args


    def main():
        args = parse_args(argv)
        exp_path = args.root_path
        dfs = []
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
        df = reduce(lambda x, y: pd.merge(x, y, on=['frame', 'sid']), dfs)


    try:
        main()
    except KeyboardInterrupt:
        print('Halted by the user')
        exit(1)
    except OSError as e:
        print('Problem reading/writing files')
        print(e)
        raise e
