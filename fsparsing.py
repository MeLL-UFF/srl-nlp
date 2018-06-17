#!/bin/env python2
"""
Script for running the Frame Semantic Parsing given some base knowledge
"""

import argparse
import logging
from ConfigParser import ConfigParser
from abc import abstractmethod
from os import path
from sys import argv as _argv
from tempfile import NamedTemporaryFile

import spacy
# from framenet.adapter import SemEval07XMLAdapter
from fol import FOL
from logger_config import timeit, config_logger, add_logger_args as _add_logger_args
from logicalform import LF
from rule_utils import get_factors
from srl_nlp.analysers.process import Process
from srl_nlp.framenet.corpus import Sentence, Annotation, AnnotationSet, Layer

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


class SemanticAnnotator(object):

    @abstractmethod
    def frameMatching(self, sentence, **params):
        pass

    @abstractmethod
    def frameElementMatching(self, sentence, annotations, **params):
        pass

    @abstractmethod
    def matching(self, sentence, **params):
        pass

    @abstractmethod
    def sem_annotations(self, sentence, sentence_lfs, anno_lfs, **kwargs):
        # type: (str, LF, list[LF], dict) -> dict[str,AnnotationSet]
        pass


class PrologBoxerAnnotator(SemanticAnnotator, Process):
    """PrologAnnotator runs a Prolog inference over the given sentence that has been processed by the analyser"""
    FRAME_RELATED_PRED = 'frame_related'
    FRAME_ELEMENT_PRED = 'frame_element'

    def __init__(self, boxer, fr_kb_file, fe_kb_file, path_to_prolog=config.get('prolog_local', 'engine')):
        self.analyser = boxer
        self.fr_kb_file = fr_kb_file
        self.fe_kb_file = fe_kb_file
        self._nlp = spacy.load(config.get('syntatic_local', 'spacy_model'))  # TODO make it an argument later
        self._get_lemma = lambda token: self._nlp(token.decode('utf-8'))[0].lemma_
        SemanticAnnotator.__init__(self)
        Process.__init__(self, path_to_prolog, True)

    def _load_file(self, file_name):
        """script method of prolog, loads specific file"""
        cmd = '[\'{f}\'].'.format(f=file_name)
        return cmd

    def _forall(self, predicate, arity=0):
        """script method of prolog, lists all groundings of this predicate"""
        vars = ','.join(['C%d' % i for i in range(arity)])
        cmd = 'forall({pred}({vars}), writeln({pred}({vars}))).'.format(pred=predicate, vars=vars)
        return cmd

    def _halt(self):
        """script method of prolog, end of script"""
        return 'halt.'

    def _script(self, *cmds):
        """script method of prolog, build the script from a sequence of commands"""
        return "\n".join(cmds)

    def _open_a_file(self, name=None):
        """Opens the file, if no name is given, opens a NamedTemporaryFile"""
        if name is not None:
            return open(name, 'wr')
        else:
            return NamedTemporaryFile()

    def _process_output(self, out):
        # """Overrides method from Process class"""
        preds = out.strip().split('\n')
        out = []
        for line in preds:
            if len(line) > 0:
                logger.debug('ProcessOutLine: \'%s\'' % line)
                lf = LF(line)
                logger.debug('ProcessOutLF:   \'%s\'' % lf)
                out.append(lf)
        return out

    def _indexes_token_in_sentence(self, token, sentence):
        """Finds out if method can be found in the sentence, uses the nlp pipeline to lemmatize

        Returns the indexes of the given token in the sentence.
        If there is no matching, returns None"""
        token = token.decode()
        tokens = self._nlp(sentence.decode())
        for s_token in tokens:
            if token == s_token.lemma_:
                return s_token.idx + len(s_token)
        return None

    def _indexes_term_in_sentence(self, link_term, lfs, sentence):
        factors = [get_factors(lf) for lf in lfs]
        for term, predicate in factors:
            if term == link_term:
                if predicate.get_pred() in self.analyser.DEFINITION_TERMS:
                    _, token = predicate.iteritems()
                    return self._indexes_token_in_sentence(token, sentence)
        return None

    def _preds2example(self, sentence, lfs, script_out):
        """Method that generates an example from the output of the prolog script"""
        lines = []
        for line in script_out.split('\n'):
            line = line.strip()
            if len(line) > 0 and not line.startswith('%'):
                lines.append(line)

        infered_lfs = [LF(line) for line in lines]
        f_set_map = dict()

        for inf_lf in infered_lfs:
            if inf_lf.get_pred() == PrologBoxerAnnotator.FRAME_RELATED_PRED:
                try:
                    link_term, frame_name, fe_name = inf_lf.iterterms()
                    assert link_term.isleaf() and frame_name.isleaf() and fe_name.isleaf()
                except ValueError as ex:  # Not correct number of predicates
                    logger.error(ex)
                    logger.error('Invalid frame element matching \'{}\''.format(inf_lf))
                    raise ex
                token_index = self._indexes_term_in_sentence(link_term, lfs, sentence)
                if token_index:
                    start, end = token_index
                    anno = Annotation(start=start, end=end, name=fe_name)
                    layer = Layer(name='FE', annotations=[anno])
                    f_set_map.setdefault(frame_name,
                                         AnnotationSet(0, frame_name=frame_name, status='auto')).layers.append(layer)

                    # logger.error('Token \'{token}\' not found in sentence \'{sent}\''.format(sent = sentence, token = token))

            if inf_lf.get_pred() == PrologBoxerAnnotator.FRAME_ELEMENT_PRED:
                try:
                    link_term, frame_name = inf_lf.iterterms()
                except ValueError as ex:  # Not correct number of predicates
                    logger.error(ex)
                    logger.error('Invalid frame matching \'{}\''.format(inf_lf))
                    raise ex
                token_index = self._indexes_term_in_sentence(link_term, lfs, sentence)
                if token_index:
                    start, end = token_index
                    anno = Annotation(start=start, end=end, name='Target')
                    layer = Layer(name='Target', annotations=[anno])
                    f_set_map.setdefault(frame_name,
                                         AnnotationSet(0, frame_name=frame_name, status='auto')).layers.append(layer)

        return Sentence(sent_id=0, text=sentence, annotation_sets=list(f_set_map.items()))

    def store_lf(self, sentence, input_file, footer='', header=''):
        # type: (str, file, str, str) -> None
        lfs = self.analyser.sentence2LF(sentence)
        input_file.write(header)
        for lf in lfs:
            for pred in lf.split():
                if pred.get_pred != FOL.OR:
                    logger.debug('PRED: {}'.format(str(pred)))
                    input_file.write(str(pred) + '\n')
        input_file.write(footer)
        input_file.flush()
        input_file.seek(0)

    def frameMatching(self, sentence, out_error=False, lf_file_name=None):
        # type: (str, bool, bool) -> (str, str)
        """

        Args:
            sentence: String, the sentence to be annotated with frame information
            out_error: If true, returns tuple (output, error), else it returns only the output
            lf_file_name: file to where store results of the annotation

        Returns:
            If out_error is false, it returns a list of frame_related predicates.
            If_error is true, then it returns a tuple with the list of predicates and the error output.

        """
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(PrologBoxerAnnotator.FRAME_RELATED_PRED, 2),
                                  self._halt())
            self.store_lf(sentence, input_file=lf_file)
            out, err = self._process(script)
            if out_error:
                return out, err
            else:
                return out

    def frameElementMatching(self, sentence, fr_anno=tuple(), lf_file_name=None, **params):
        # type: (str, list, bool, str, any) -> tuple[str,str]
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(PrologBoxerAnnotator.FRAME_ELEMENT_PRED, 3),
                                  self._halt())
            self.store_lf(sentence, header='\n'.join(map(str, fr_anno)) + '\n', input_file=lf_file)
            out, err = self._process(script)

            return out, err

    def matching(self, sentence, lf_file_name=None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(PrologBoxerAnnotator.FRAME_RELATED_PRED, 2),
                                  self._forall(PrologBoxerAnnotator.FRAME_ELEMENT_PRED, 3),
                                  self._halt())
            self.store_lf(sentence, input_file=lf_file)
            out, err = self._process(script)
        return out, err

    def sem_annotations(self, sentence, sentence_lfs=None, anno_lfs=None, var2pos_list=None, **kwargs):
        """Gets the annotations in f

            Args:
                sentence: sentence that is going to be matched
                sentence_lfs: list of logical form representations of the sentence
                anno_lfs: predicate list
                var2pos_list: dictionary mapping variable string to list of tuples (start,end)
                **kwargs: Other parameter for compatibility. Ignored.

            Returns:
                A dictionary of frames names and annotation sets
        """

        # TODO change frame_element(Var, FE, Frame) to frame_element(Var, FE, Var_frame)
        # TODO implement the interval closing
        # Get dictionaries to perform matching
        if not anno_lfs:
            return dict()
        if not sentence_lfs:
            sentence_lfs = self.analyser.sentence2LF(sentence)
        if var2pos_list is None:
            token2pos = self.analyser.get_matching_tokens(sentence, 'pos')
            var2pos_list = self.get_matching_variables(sentence, sentence_lfs, token2pos)
        # Run through lfs to get the info
        frs = dict()
        count = 0
        for lf in anno_lfs:
            for pred in lf.split():
                for child in pred.iterterms():
                    child_pred = child.get_pred()
                    if child_pred in var2pos_list:
                        if pred.get_pred() == 'frame_related':
                            frame_name = pred.info[-1][0]
                            frame = frs.get(frame_name, AnnotationSet(anno_set_id="%07d" % count, frame_name=frame_name, status="Breno"))
                            layer = Layer(name="Target")
                            for start, end in var2pos_list[child_pred]:
                                layer.annotations.append(Annotation(name="Target", start=start, end=end))
                            frame.layers.append(layer)
                            frs[frame_name] = frame
                            count = count + 1
                        break

        for lf in anno_lfs:
            for pred in lf.split():
                for child in pred.iterterms():
                    child_pred = child.get_pred()
                    if child_pred in var2pos_list:
                        if pred.get_pred() == 'frame_element':
                            frame_name = pred.info[3][0]
                            fe_name = pred.info[2][0]
                            if frame_name in frs:
                                frame = frs[frame_name]
                                layers = [layer for layer in frame.layers if layer.name == "FE"]
                                if len(layers) > 0:
                                    layer = layers[0]
                                else:
                                    layer = Layer(name="FE")
                                    frame.layers.append(layer)
                                for start, end in var2pos_list[child_pred]:
                                    layer.annotations.append(Annotation(start=start, end=end, name=fe_name))
                            else:
                                # TODO better handle this exception
                                # raise Exception("Oh fork, this frame element ({fe}) is orphan.".format(fe=fe_name))
                                logger.error("Oh fork, this frame element ({fe}) is orphan.".format(fe=fe_name))
                                break
        return frs

    def get_matching_variables(self, sentence, sentence_lfs=None, matching=None):
        # type: (str, list[LF],dict[any,any]) -> dict[LF,list[any]]
        if not sentence_lfs:
            sentence_lfs = self.analyser.sentence2LF(sentence)
        if not matching:
            matching = self.analyser.get_matching_tokens(sentence, output="pos")
        matching1 = dict()
        for lf in sentence_lfs:
            for pred in lf.split():
                for child in pred.iterterms():
                    if child.get_pred() in matching:
                        term = pred.iterterms().next().get_pred()
                        tmp_list = matching1.get(term, [])
                        tmp_list.append(matching[child.get_pred()])
                        matching1[term] = tmp_list
                        break
        return matching1


def parse_args(argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='Runs the Boxer analysis and then the frame parsing on the sentence')
    # parser.add_argument('dir_path', help = 'the path of the experiments')

    parser.add_argument('sentence',
                        help='the sentence to be matched')
    parser.add_argument('-t', '--tmp_lf_file',
                        help='save lf generated, for inspection')
    parser.add_argument('-f', '--frame_matching',
                        action='store_true', default=False,
                        help='show the frame matching process')
    parser.add_argument('-e', '--frame_element_matching',  # TODO pass frame related annotations
                        action='store_true', default=False,
                        help='show the frame element matching process')
    parser.add_argument('-m', '--matching',
                        action='store_true', default=False,
                        help='show the matching of both')

    parser.add_argument('-x', '--eval_format',
                        action='store_true', default=False,
                        help='return the matchings in the SemEval format')

    parser.add_argument('-K', '--kb_path', default='.',
                        help='path to knowledge base files')
    parser.add_argument('-E', '--kb_fe', default='kb_fe',
                        help='relative path to frame element knowledge base')
    parser.add_argument('-R', '--kb_fr', default='kb_fr',
                        help='relative path to path to frame matching knowledge base')
    # parser.add_argument('-i', '--stdin', action='store_true', default=False, help = 'increase output verbosity')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


@timeit
def main(argv):
    args = parse_args(argv, _add_logger_args)
    config_logger(args)
    logger.info('Starting')

    from srl_nlp.analysers.boxer import BoxerLocalAPI
    boxer = BoxerLocalAPI()
    kb_fr_path = path.join(args.kb_path, args.kb_fr)
    kb_fe_path = path.join(args.kb_path, args.kb_fe)
    anno = PrologBoxerAnnotator(boxer, kb_fr_path, kb_fe_path)

    print 'LF: %s' % boxer.sentence2LF(args.sentence)

    if args.frame_matching:
        print 'Frame Matching:'
        out, err = anno.frameMatching(args.sentence, out_error=True, lf_file_name=args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' % '\n'.join(map(str, out))

    if args.frame_element_matching:
        print '\nFrame Element Matching:'
        out, err = anno.frameElementMatching(args.sentence, out_error=True, lf_file_name=args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' % '\n'.join(map(str, out))

    if args.matching:
        print '\nMatching:'
        out, err = anno.matching(args.sentence, lf_file_name=args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' % '\n'.join(map(str, out))
        # semeval_parser = SemEval07XMLAdapter()
        # for f_name, annoset in anno.sem_annotations(args.sentence, out).items():
        #     print "Frame:{f_name}\n\t{anno}".format(f_name=f_name, anno=semeval_parser._anno_set2XML(annoset))

    logger.info('Done')


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
