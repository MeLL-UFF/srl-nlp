import logging
import random
from itertools import chain
from sys import argv as _argv

import pickle
import re
import spacy
from os import path, makedirs
from srl_nlp.framenet.adapter import PARSERS_AVAILABLE

logger = logging.getLogger(__name__)


class DataObject(object):

    def __init__(self, s_id, frs, fes, preds):
        # type: (str, list[list[str]], list[list[str]], list[str]) -> None
        self.s_id = s_id
        self.frs = frs
        self.fes = fes
        self.preds = preds

    def __repr__(self):
        return "DataObject({})".format(self.s_id)


def get_tree_in(dep_tree, start, end):
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
                if layer.name == 'Target':
                    fr = []
                    for word in highlight:
                        fr.append('frame_token({S}, c_{C:03d}, "{F}").'.format(S=s_id, C=word.i, F=frame_name))
                    if len(fr) > 0:
                        frs.append(fr)
                elif layer.name == 'FE':
                    fe = []
                    for word in highlight:
                        name = anno.name.lower()
                        fe.append('frame_element_token({S}, c_{C:03d}, "{FE}", "{F}").'
                                  .format(S=s_id, C=word.i, FE=name, F=frame_name))
                    if len(fe) > 0:
                        fes.append(fe)
    return frs, fes


def tree_to_dep_facts(sentence_tree, s_id, encoding='utf-8'):
    for word in sentence_tree:
        values = {"pos": word.pos_.lower(),
                  "s_id": s_id,
                  "c_id": word.i,
                  "text": word.text.encode(encoding).replace("'", "\\'").lower(),
                  "dep": word.dep_.lower(),
                  "hc_id": word.head.i}
        yield '{pos}({s_id}, c_{c_id:03d}, "{text}").'.format(**values)
        if word.head != word:
            yield '{dep}({s_id}, c_{c_id:03d}, c_{hc_id:03d}).'.format(**values)
        else:
            yield 'sentence_root({s_id}, c_{c_id:03d}).'.format(**values)

def tree_to_lex_facts(sentence_tree, s_id, encoding='utf-8'):
    for word in sentence_tree:
        text= word.text.encode(encoding).replace("'", "\\'").lower()
        yield 'token({s_id}, l_{c_id:03d}, "{text}").'.format(s_id = s_id,
                                                              c_id = word.i,
                                                              text = text)


def examples_from_doc(spacy_parser, d_id, doc, encoding = 'utf-8', unify_ids=True):
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
                                   tree_to_lex_facts(sentence_tree, s_id, encoding=encoding)))

                yield DataObject(s_id, frs, fes, preds)
            except (AssertionError, IOError) as ex:
                logger.error(ex.message)
                print ex
            # break


def write_to_file(root_folder, dataset, dataset_name):
    # type: (str, list[DataObject], str) -> None
    pattern = re.compile(r"(frame_element_token\(.*?,)\s*(c_\d+)\s*(,.*)")
    def get_vars(frame_elements, pattern = pattern):
        for frame_element in frame_elements:
            line = str(frame_element)
            logger.debug("[{ds}] Line: '{line}' was parsed"
                         .format(line=line, ds=dataset_name))
            match = pattern.match(line)
            if match:
                yield match.group(2)
            else:
                logger.warning("[{ds}] Line: '{line}' is not a FET"
                               .format(line=line, ds=dataset_name))
                pass

    def replace_var_iter(string, var_set, pattern = pattern):
        match = pattern.match(string)
        if match:
            preffix, const, suffix = match.groups()
            for var in var_set:
                yield "".join([preffix, var, suffix])
        else:
            logger.warning("Line: '{line}' is not a FET".format(line = string))

    folder = path.join(root_folder, dataset_name)
    logger.info('Root: {f}'.format(f=folder))
    if not path.exists(folder):
        makedirs(folder)
    with open(path.join(folder, dataset_name + '_facts.txt'), 'w') as fact_f:
        with open(path.join(folder, dataset_name + '_pos.txt'), 'w') as pos_f:
            with open(path.join(folder, dataset_name + '_neg.txt'), 'w') as neg_f:
                for data_obj in dataset:
                    # FACTS
                    all_frs = list(chain(*data_obj.frs))
                    logger.info('Writing {ds} facts'.format(ds=dataset_name))
                    fact_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    fact_f.write("\n".join(all_frs))
                    fact_f.write("\n")
                    fact_f.write("\n".join(data_obj.preds))
                    fact_f.write("\n\n")

                    all_fes = list(chain(*data_obj.fes))

                    # POSITIVE EXAMPLES
                    logger.info('Writing {ds} positive examples'.format(ds=dataset_name))
                    pos_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    pos_f.write("\n".join(all_fes))
                    pos_f.write("\n\n")

                    # NEGATIVE EXAMPLES

                    all_vars = set(get_vars(all_fes))
                    logger.info('Writing {ds} negative examples'.format(ds=dataset_name))
                    neg_f.write("% sentence {s}\n".format(s=data_obj.s_id))
                    for fes in data_obj.fes:
                        pos_vars = get_vars(fes)
                        for out in replace_var_iter(fes[0], all_vars.difference(pos_vars)):
                            neg_f.write(out)
                            neg_f.write('\n')
                        neg_f.write("\n")


def get_examples(data_base_path, input_parser):
    with open(data_base_path) as db_file:
        docs = input_parser.parseXML(db_file)
    logger.info('Done parsing')
    logger.info('Creating base')
    examples = []
    model = 'en_core_web_sm'
    spacy_parser = spacy.load(model)

    for d_id, doc in enumerate(docs[:1]):
        for example in examples_from_doc(spacy_parser, d_id, doc, unify_ids=True):
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
        parser.add_argument('-p', '--pickle_file',
                            help='store intermediate results or read from it if file exists')

        parser.add_argument('-t', '--train_ratio', default=None, type=int,
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

        # Select the appropriate document parsers for the input and output
        input_parser = PARSERS_AVAILABLE[args.input_format]()
        logger.info('Parsing {file}'.format(file=args.data_base_path))

        if args.pickle_file:
            if path.exists(args.pickle_file):
                # Load examples from pickle file
                logger.info('Loading examples from {file}'.format(file=args.pickle_file))
                with open(args.pickle_file, 'r') as f:
                    examples = pickle.load(f)
            else:
                # Parse corpus
                examples = get_examples(args.data_base_path, input_parser)
                # Save pickle file
                with open(args.pickle_file, 'w') as f:
                    pickle.dump(examples, f)
        else:
            # Parse corpus
            examples = get_examples(args.data_base_path, input_parser)

        # Split dataset in training and test if train_ratio is given
        if args.train_ratio:
            # shuffle examples
            random.Random(args.seed).shuffle(examples)

            len_train = (args.train_ratio * len(examples) / (args.train_ratio + 1))
            train, test = examples[:len_train], examples[len_train:]

            write_to_file(args.root, train, 'train')
            write_to_file(args.root, test, 'test')
        else:
            write_to_file(args.root, examples, 'train')
        logger.info('Done')


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
