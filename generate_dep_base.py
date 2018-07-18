import json
import logging
import pickle
import random
from collections import Iterable
from itertools import chain
from sys import argv as _argv

import spacy
from analysers.boxer import BoxerLocalAPI
from os import path, makedirs
from srl_nlp.framenet.adapter import PARSERS_AVAILABLE

logger = logging.getLogger(__name__)


class FrameAnno:
    def __init__(self, s_id, start, end, frame):
        self.s_id = s_id
        self.start = start
        self.end = end
        self.frame = frame

    def __str__(self):
        return 'frame_anno({S}, c_{C1:03d}, c_{C2:03d}, \'{F}\').' \
            .format(S=self.s_id, C1=self.start, C2=self.end, F=self.frame)

    def __repr__(self):
        return str(self)


class FrameElementAnno:
    def __init__(self, s_id, start, end, frame_element, frame):
        self.s_id = s_id
        self.start = start
        self.end = end
        self.frame_element = frame_element
        self.frame = frame

    def __str__(self):
        return 'frame_element_anno({S}, c_{C1:03d}, c_{C2:03d}, \'{FE}\', \'{F}\').' \
            .format(S=self.s_id, C1=self.start, C2=self.end, FE=self.frame_element, F=self.frame)

    def __repr__(self):
        return str(self)


class DataObject(object):
    """
    Representation of a datum of this project.
    It contains a list of frames, a list of frame elements and a list of facts
    (preds), all of those as strings in Prolog facts.
    """

    def __init__(self, s_id, frs, fes, preds, num_tokens, analyser_preds):
        # type: (str, list[FrameAnno], list[FrameElementAnno], list[str]) -> None
        self.s_id = s_id  # type: str
        self.frs = frs  # type: list(FrameAnno)
        self.fes = fes  # type: list(FrameElementAnno)
        self.preds = preds  # type: list(str)
        self.analyser_preds = analyser_preds  # type: list(str)
        self.num_tokens = num_tokens  # type: int

    def __repr__(self):
        return "DataObject({})".format(self.s_id)


def get_tree_in(dep_tree, start, end):
    """
    Get sub-sentence defined by start and end.

    Args:
        dep_tree: spacy dep_tree
        start: initial character position of the sub-sentence
        end: end character position of the sub-sentence

    Yields:
        The words in the tree that are inside the closed interval defined by start and end

    """
    for word in dep_tree:
        if word.idx >= start:
            if word.idx + len(word) > end + 1:
                break
            yield word


def get_annotations(sentence, sentence_tree, s_id):
    """
    Get the annotations from the corpus sentence

        Args:
            sentence: Sentence object from a parsed corpus
            sentence_tree:
            s_id: string used to identify the LFs generated

        Returns:
            Two lists.
            The first list contains the frame matching, the second one are the fe matching
    """
    logger.debug("Sentence: {sent}".format(sent=sentence))
    frs = []
    fes = []
    for anno_set in sentence:
        for layer in anno_set:
            for anno in layer:
                highlight = list(get_tree_in(sentence_tree, anno.start, anno.end))
                frame_name = anno_set.frameName.lower()
                if len(highlight) > 0:
                    start = highlight[0]
                    end = highlight[-1]
                    if layer.name == 'Target':
                        frs.append(FrameAnno(s_id=s_id,
                                             start=start.i,
                                             end=end.i,
                                             frame=frame_name))
                    elif layer.name == 'FE':
                        name = anno.name.lower()
                        fes.append(FrameElementAnno(s_id=s_id,
                                                    start=start.i,
                                                    end=end.i,
                                                    frame_element=name,
                                                    frame=frame_name))
    return frs, fes


def tree_to_dep_facts(sentence_tree, s_id, encoding='utf-8', lemmatize=True):
    for word in sentence_tree:
        if lemmatize:
            raw_text = word.lemma_
        else:
            raw_text = word.text
        text = raw_text.encode(encoding).replace("'", "\\'").lower()

        values = {"pos": word.pos_.lower(),
                  "s_id": s_id,
                  "c_id": word.i,
                  "text": text,
                  "dep": word.dep_.lower(),
                  "hc_id": word.head.i}
        yield 'pos_{pos}({s_id}, c_{c_id:03d}, \'{text}\').'.format(**values)
        if word.head != word:
            yield '{dep}({s_id}, c_{c_id:03d}, c_{hc_id:03d}).'.format(**values)
        else:
            yield 'sentence_root({s_id}, c_{c_id:03d}).'.format(**values)


def tree_to_lex_facts(sentence_tree, s_id):  # , encoding='utf-8'):
    c_ids = ','.join(['c_{:03d}'.format(word.i) for word in sentence_tree])
    yield 'tokens({s_id}, [{c_ids}]).'.format(s_id=s_id, c_ids=c_ids)

    # for word in sentence_tree:
    #     text = word.text.encode(encoding).replace("'", "\\'").lower()
    #     yield 'token({s_id}, c_{c_id:03d}, "{text}").'.format(s_id=s_id,
    #                                                           c_id=word.i,
    #                                                           text=text)


def examples_from_doc(spacy_parser, d_id, doc, analyser=None, encoding='utf-8', unify_ids=True):
    logger.info("Reading docs")

    for paragraph in doc.elements:
        logger.info("Reading paragraph id({par_id})".format(par_id=paragraph.id))

        for sentence in paragraph.sentences:
            logger.info("Reading sentence id({par_id}:{sent_id})"
                        .format(par_id=paragraph.id, sent_id=sentence.id))
            try:
                if unify_ids:
                    s_id = 's{d:03}_{p}_{s}'.format(d=d_id, p=paragraph.id, s=sentence.id)
                else:
                    s_id = 's_{s_id}'.format(s_id=sentence.id)
                sentence_tree = spacy_parser(sentence.text.decode(encoding))
                frs, fes = get_annotations(sentence, sentence_tree, s_id)
                preds = list(chain(tree_to_dep_facts(sentence_tree, s_id, encoding=encoding),
                                   tree_to_lex_facts(sentence_tree, s_id)))  # , encoding=encoding)))

                a_preds = []
                if analyser is not None:
                    lfs = analyser.sentence2LF(sentence.text, const_prefix='b_')
                    for lf in lfs:
                        for pred in lf.split():
                            # process pred and then append it
                            pred.info[1:1] = [[s_id]]
                            pred_str = str(pred)
                            if not pred_str.startswith('(') and not pred_str.startswith('not'):
                                a_preds.append(pred_str)

                yield DataObject(s_id, frs, fes, preds, len(sentence_tree), analyser_preds=a_preds)
            except (AssertionError, IOError) as ex:
                logger.error(ex.message)
                print ex
            # break


def all_subsentences(total_tokens, to_str=False):
    def parse(x):
        return "c_{:03d}".format(x)

    for start in range(total_tokens):
        for end in range(start, total_tokens):
            if to_str:
                yield (parse(start), parse(end))
            else:
                yield (start, end)


def get_unique_fes(fes, fe_dict, random, extra_frames):
    if len(fes) != 0:
        if fe_dict is not None:
            frames = set(fe.frame for fe in fes)
            keys = set(fe_dict.keys())
            for _ in range(extra_frames):
                frames.add(random.choice(list(keys.difference(frames))))
            unique_fes = set()
            for frame in frames:
                if frame in fe_dict:
                    for fe in fe_dict[frame]:
                        unique_fes.add((fe, frame))
            pass
        else:
            unique_fes = set((fe.frame_element, fe.frame) for fe in fes)
        return unique_fes


def get_unique_fes_all(fes, fe_dict, random, extra_frames):
    if len(fes) != 0:
        if fe_dict is not None:
            frames = set(fe.frame for fe in fes)
            keys = set(fe_dict.keys())
            for _ in range(extra_frames):
                frames.add(random.choice(list(keys.difference(frames))))
            all_fes = set(chain(*[fe_dict.get(frame, []) for frame in frames]))
            unique_fes = set()
            for frame in frames:
                for fe in all_fes:
                    unique_fes.add((fe, frame))
            pass
        else:
            unique_fes = set((fe.frame_element, fe.frame) for fe in fes)
        return unique_fes


def neg_examples_gen(fes, all_pairs, fe_dict=None, random=random.Random(), extra_frames=0):
    # type: (list[FrameElementAnno], set[tuple[str,str]]) -> Iterable[FrameElementAnno]
    if len(fes) != 0:
        unique_fes = get_unique_fes_all(fes, fe_dict, random, extra_frames)
        pos = set(fes)
        s_id = fes[0].s_id
        for fe, frame in unique_fes:
            for start, end in all_pairs:
                new_anno = FrameElementAnno(s_id, start=start,
                                            end=end,
                                            frame=frame,
                                            frame_element=fe)
                if new_anno not in pos:
                    yield new_anno


def write_to_file(root_folder, dataset, dataset_name, fact_header=None, max_np_ratio=None, fe_dict=None,
                  extra_neg_frames=1, seed=None):
    # type: (str, list[DataObject], str) -> None
    """
    Creates the files in the RDN format

    Args:
        root_folder: Folder to store the dataset folder
        dataset: dataset to be used to write the files
        dataset_name: dataset and folder name
        fact_header: header for the fact files, its ok to have "\n"
        max_np_ratio: Maximum ratio negative/positive examples a sentence will have
        seed: Random seed to replicate the experiments. If None, use no fixed seed.
        extra_neg_frames: TODO
        fe_dict: TODO

    Returns:
        None. It write the files in the specified directory
    """
    folder = path.join(root_folder, dataset_name)
    logger.info('Root: {f}'.format(f=folder))
    if not path.exists(folder):
        makedirs(folder)
    logger.info('Writing {ds} files'.format(ds=dataset_name))

    random_gen = random.Random(seed)

    with open(path.join(folder, dataset_name + '_facts.txt'), 'w') as fact_f:
        with open(path.join(folder, dataset_name + '_pos.txt'), 'w') as pos_f:
            with open(path.join(folder, dataset_name + '_neg.txt'), 'w') as neg_f:

                if fact_header is not None:
                    fact_f.write(fact_header.replace('\\n', '\n'))
                    fact_f.write("\n")

                for data_obj in dataset:
                    # FACTS
                    logger.debug('Writing {ds}:{s} facts'.format(ds=dataset_name, s=data_obj.s_id))
                    fact_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    fact_f.write("\n".join(map(str, data_obj.frs)))
                    fact_f.write("\n")
                    fact_f.write("\n".join(map(str, data_obj.preds)))
                    fact_f.write("\n%analyser preds")
                    fact_f.write("\n".join(map(str, data_obj.analyser_preds)))
                    fact_f.write("\n\n")

                    # POSITIVE EXAMPLES
                    logger.debug('Writing {ds}:{s} positive examples'.format(ds=dataset_name, s=data_obj.s_id))
                    pos_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    pos_f.write("\n".join(map(str, data_obj.fes)))
                    pos_f.write("\n\n")

                    # NEGATIVE EXAMPLES

                    # all_vars =  set((fe.start, fe.end) for fe in data_obj.fes)
                    all_vars = set(all_subsentences(data_obj.num_tokens))
                    logger.debug('Writing {ds}:{s} negative examples'.format(ds=dataset_name, s=data_obj.s_id))
                    neg_f.write("% sentence {s}\n".format(s=data_obj.s_id))

                    neg_examples = neg_examples_gen(data_obj.fes, all_pairs=all_vars, fe_dict=fe_dict,
                                                    random=random_gen, extra_frames=extra_neg_frames)
                    if max_np_ratio:
                        neg_examples_tmp = list(neg_examples)
                        qtd = min(int(len(data_obj.fes) * max_np_ratio), len(neg_examples_tmp))
                        indexes = xrange(len(neg_examples_tmp))
                        neg_examples = [neg_examples_tmp[i]
                                        for i in random_gen.sample(indexes, qtd)]
                    for neg_exe in neg_examples:
                        neg_f.write(str(neg_exe))
                        neg_f.write('\n')
                    neg_f.write('\n')


def get_examples(data_base_path, input_parser, use_boxer=False):
    with open(data_base_path) as db_file:
        docs = input_parser.parseXML(db_file)
    logger.info('Done parsing')
    logger.info('Creating base')
    examples = []
    model = 'en_core_web_sm'
    spacy_parser = spacy.load(model)

    analyser = BoxerLocalAPI() if use_boxer else None

    for d_id, doc in enumerate(docs):
        for example in examples_from_doc(spacy_parser, d_id, doc, analyser=analyser, unify_ids=True):
            examples.append(example)
    return examples


if __name__ == '__main__':
    import argparse
    from logger_config import add_logger_args as _add_logger_args, config_logger, timeit


    def parse_args(argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Creates a base using dep tree')
        parser.add_argument('data_base_path',
                            help='path to the semeval dataset folder')
        parser.add_argument('-i', '--input_format',
                            choices=PARSERS_AVAILABLE.keys(),
                            default=PARSERS_AVAILABLE.keys()[-1],
                            help='input annotation file format')

        parser.add_argument('-R', '--root',
                            help='relative path to path to store the bases')
        parser.add_argument('-N', '--max_np_ratio', type=int, default=None,
                            help='Maximum ratio negative/positive examples a sentence will have')
        parser.add_argument('-H', '--header',
                            help='header of the fact files')
        parser.add_argument('-p', '--pickle_file', default='',
                            help='store intermediate results or read from it if file exists')

        parser.add_argument('-j', '--fe_json_file', default=None,
                            help='path to the json file with dictionary of frames to fe')

        parser.add_argument('-b', '--use_boxer', default=False, action='store_true',
                            help='Return boxer predicates')
        parser.add_argument('-x', '--extra_neg_frames', default=1, type=int,
                            help='add [extra_neg_frames] extra random frames to each'
                                 'negative set of examples for sentence')

        sp = parser.add_mutually_exclusive_group()
        sp.add_argument('-f', '--subfolders', action='store_true', default=False,
                        help='if there are two train and test files, use it')
        sp.add_argument('-t', '--train_ratio', default=None, type=int,
                        help='optional train/test ratio')

        parser.add_argument('-s', '--seed', default=None, type=int,
                            help='optional random seed')

        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        return args


    @timeit
    def main(argv):
        args = parse_args(argv, _add_logger_args)
        config_logger(args)

        logger.info(str(args))
        logger.info('Initialization')
        loaded = False

        if path.exists(args.pickle_file):
            # Load examples from pickle file
            logger.info('Loading examples from {file}'.format(file=args.pickle_file))
            with open(args.pickle_file, 'r') as f:
                train, test = pickle.load(f)
            loaded = True
        else:
            # Select the appropriate document parsers for the input and output
            input_parser = PARSERS_AVAILABLE[args.input_format]()
            logger.info('Parsing {file}'.format(file=args.data_base_path))
            train, test = get_train_test(args, input_parser)

        if args.pickle_file and not loaded:
            # Save pickle file
            with open(args.pickle_file, 'w') as f:
                pickle.dump((train, test), f)

        if args.fe_json_file is not None:
            with open(args.fe_json_file, 'r') as d_file:
                fe_dict = json.load(d_file)
        else:
            fe_dict = None

        # Split dataset in training and test if train_ratio is given
        write_to_file(args.root, train, 'train', fact_header=args.header,
                      max_np_ratio=args.max_np_ratio, fe_dict=fe_dict,
                      extra_neg_frames=args.extra_neg_frames)
        write_to_file(args.root, test, 'test', fact_header=args.header,
                      max_np_ratio=args.max_np_ratio, fe_dict=fe_dict,
                      extra_neg_frames=args.extra_neg_frames)
        logger.info('Done')


    def get_train_test(args, input_parser):
        if args.train_ratio:
            # Parse corpus
            examples = get_examples(args.data_base_path, input_parser, use_boxer=args.use_boxer)
            # shuffle examples
            random.Random(args.seed).shuffle(examples)

            len_train = (args.train_ratio * len(examples) / (args.train_ratio + 1))
            train, test = examples[:len_train], examples[len_train:]
        elif args.subfolders:
            train_path = path.join(args.data_base_path, 'train.xml')
            test_path = path.join(args.data_base_path, 'test.xml')
            train = get_examples(train_path, input_parser, use_boxer=args.use_boxer)
            test = get_examples(test_path, input_parser, use_boxer=args.use_boxer)
        else:
            train = get_examples(args.data_base_path, input_parser, use_boxer=args.use_boxer)
            test = []
        return train, test


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
