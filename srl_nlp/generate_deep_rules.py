#!/bin/env python2
"""
This script creates a simple theory from FrameNet that can be further revised.
"""

__author__ = "Breno W. Carvalho"
__copyright__ = "Copyright 2018"
__credits__ = ["Breno W. Carvalho", "Bernardo Goncalves", "Aline Marins Paes"]
__email__ = "brenocarvalho@ic.uff.br"
__status__ = "Prototype"

# Utils
import argparse
# Logger
import logging
import pickle
from ConfigParser import ConfigParser
from functools import partial
from os import path
from sys import argv as _argv
from sys import stdout

from logger_config import config_logger, add_logger_args as _add_logger_args
# Analysers
from srl_nlp.analysers.boxer import BoxerLocalAPI
from srl_nlp.framenet.parse_xml import NetXMLParser
from srl_nlp.rule_utils import spacy, get_annotations, get_factors, make_pred
from srl_nlp.rule_utils import str_preds, get_paths, get_abbrev, get_examples

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


class RuleGenerator(object):
    """Take Examples and convert them to rules"""
    FE_PRED = 'frame_element'
    FR_PRED = 'frame_related'

    def __init__(self, *analysers):
        self.analysers = analysers
        nlp = spacy.load('en_core_web_sm')
        self._get_lemma = lambda token: nlp(token.decode('utf-8'))[0].lemma_

    def get_rules(self, frame, *examples):
        """Generates FrameElement rules"""
        x = (self.FR_PRED, self.FE_PRED, 'relation')
        for analyser in self.analysers:
            for example in examples:
                lfs = analyser.sentence2LF(example.str_no_annotation())
                for lf in lfs:
                    if not lf.has_pred('not'):
                        logger.debug('Path:%s' % str(lf))
                        try:
                            elements, targets = get_annotations(example, lf, get_abbrev(frame),
                                                                get_lemma=self._get_lemma)
                            logger.debug('Done Annotations:%s' % str(targets))
                            factors = get_factors(lf)
                            logger.debug('Done Factors:%s' % str(factors))
                            for target in targets:
                                for label, preds in elements.iteritems():
                                    for pred in preds:
                                        for term_path in get_paths(pred, target, factors):
                                            if label[:1].islower():
                                                logger.error(example)
                                                logger.error(example)
                                                logger.error(term_path)
                                                logger.error(elements)
                                                logger.error(targets)
                                                raise Exception(
                                                    '%s FE was not matched, the label is lower cased' % label)
                                            rule = "%s :- %s, %s." % (
                                                str_preds(make_pred(self.FE_PRED,
                                                                    pred,
                                                                    label.lower(),
                                                                    frame.name.lower()), x=x),
                                                str_preds(make_pred(self.FR_PRED,
                                                                    target,
                                                                    frame.name.lower()), x=x),
                                                str_preds(term_path, x=x))
                                            logger.debug("Rule: %s" % rule)
                                            yield rule
                                            break
                        except IndexError as ex:
                            logger.error(ex)
                            continue


def make_theory(rule_generator, fe_examples):
    """
    Returns an iterator of strings where each string is a ProLog rule.
    """
    for idx, i in enumerate(fe_examples):
        examples, frame = i
        logger.info('[{idx}] example | frame:  {i}'.format(idx=idx, i=str(i)))
        if len(examples) > 0:
            logger.info('Example: %s' % str(examples[0].str_no_annotation()))
        for rule in rule_generator.get_rules(frame, *examples):
            yield rule
    logger.info('Made theory')


############################

# Get Frame Matching Rules #

############################

lu_pos2pred = {'A': 'adjective',
               # 'ADV'  : '', #adverb
               # 'ART'  : '', #quantity modifier
               # 'C'    : '', #conjuction
               # 'IDIO' : '', #idiom
               # 'INTJ' : '', #interjection
               'N': 'noun',
               'NUM': 'number',
               # 'PREP' : '', #preposition
               # 'PRON' : '', #pronoun
               # 'SCON' : '', #? conditional
               'V': 'verb'}


def get_lus2frames(framenet):
    """
    Returns a mapping of Lexical Units to sets of Frames in all FrameNet

    Args:
        framenet: a FrameNet Object (any version of it from this package will work)

    Returns:
        Dctionary of the form of Lexical Units as keys and sets of Frames related to the LU as values.

    """
    lus2frames = dict()
    for frame in framenet:
        for lu in frame.LUs:
            s = lus2frames.get(lu, set())
            s.add(frame)
            lus2frames[lu] = s
    return lus2frames


def make_frame_matching_rules(lus2frames, lu_pos_to_pred=lu_pos2pred, f_out=None):
    """
    Method to generate simple Frame Matching rules based on Lexical Units

    Args:
        lus2frames:
        lu_pos_to_pred:
        f_out: file to receive the list of rules

    Returns:
        If f_out is not None, returns the list of rues that matches frames to sentences.
        Otherwise, write it to f_out and returns Nothing
    """
    out = []

    def handle_name(name):
        return name.replace('-', '_').replace(' ', '_').lower()

    def make_rule(lexical_unit, frame):
        return "frame_related(X, {frame})" \
               ":- {pos}(X, {token}).".format(frame=handle_name(frame.name),
                                              pos=lu_pos_to_pred[lexical_unit.pos],
                                              token=handle_name(lexical_unit.name))

    for lu in lus2frames.keys():
        for frame in lus2frames[lu]:
            try:
                if f_out:
                    f_out.write(make_rule(lu, frame))
                    f_out.write('\n')
                else:
                    out.append(make_rule(lu, frame))
            except KeyError:
                logger.warning('Not supported part of speech tag of this LU: %s' % lu)
                pass
    if not f_out:
        return out


def get_all_examples(fn, example_file_name=None):
    """
    Tries to read the examples from file or to generate them from the FrameNet fn if no file is found.

    Args:
        fn: a FrameNet Object (any version of it from this package will work)
        example_file_name: file to try to read the examples from

    Returns:
        List of examples. An example is a pair (list of Frame Element Examples, Frame).

    """
    examples = []
    if example_file_name:
        try:
            logger.info('Read examples from %s' % example_file_name)
            with open(example_file_name, 'r') as f:
                examples = pickle.load(f)
        except IOError as ex:
            logger.error('Exception reading \'%s\': %s' % (example_file_name, ex))
    else:
        logger.info('Capturing examples')
        examples = []
        for l in list(map(partial(get_examples, fn=fn), fn.fe_names)):
            examples.extend(l)
    return examples


##########################

def parse_args(argv=_argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='KB generator. Writes the union of the set of the deep '
                                                 'role rules and the set of the frame matching rules.')
    parser.add_argument('out_file',
                        help='The path to where to write the rules. '
                             'If none, the rules are going to be printed on screen')
    parser.add_argument('--example_file', help='The path to where to write/read examples TODO')
    parser.add_argument('-l', '--limit', help='Max number of examples used')
    parser.add_argument('-r', '--frame_related', action='store_true', default=False,
                        help='Create frame_related rules')
    parser.add_argument('-e', '--frame_element', action='store_true', default=False,
                        help='Create frame_element rules')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


def main(argv):
    """
    Process the arguments in order to return the theory.
    The theory is the union of the set of the deep role rules and the
    set of the frame matching rules.
    """
    args = parse_args(argv, _add_logger_args)
    config_logger(args)
    logger.info(args)
    logger.info('Starting FrameNet')
    parser = NetXMLParser()
    fn = parser.parse('framenet/fndata-1.7')

    if args.frame_related:
        logger.info('Initialization of frame matching rule inference')
        frame_matching_rules = make_frame_matching_rules(get_lus2frames(fn))

        if args.out_file:
            logger.info('Saving to %s' % args.out_file)
            stream = open(args.out_file, 'a')
        else:
            logger.info('Writing to stdout')
            stream = stdout
        with stream as f:
            f.write("%% Frame Matching Rules\n")
            for rule in frame_matching_rules:
                f.write(rule)
                f.write('\n')

    ########################
    #                      #
    #  Frame Element Rules #
    #                      #
    ########################
    if args.frame_element:
        examples = get_all_examples(fn, args.example_file)

        logger.info('Starting Boxer')
        boxer = BoxerLocalAPI(expand_predicates=True)
        logger.info('Boxer started')
        if args.limit:
            sliced_examples = examples[:int(args.limit)]
        else:
            sliced_examples = examples
        logger.info('There are %d examples being considered' % len(sliced_examples))
        theory = make_theory(RuleGenerator(boxer), sliced_examples)

        if args.out_file:
            logger.info('Saving to %s' % args.out_file)
            stream = open(args.out_file, 'a')
        else:
            stream = stdout
        with stream as f:
            f.write("\n%% Deep Role Rules\n")
            for rule in theory:
                f.write(rule)
                f.write('\n')


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
        raise e
