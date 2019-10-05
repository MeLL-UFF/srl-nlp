#!/bin/env python2
"""
Script for running the Frame Semantic Parsing given some base knowledge
"""
from sys import argv as _argv

import argparse
import json
import logging
import spacy
from configparser import ConfigParser
from abc import abstractmethod
from os import path, remove as remove_file
from tempfile import NamedTemporaryFile
from typing import List, Dict, Tuple

from srl_nlp.data_augmentation.analysers.process import Process
from srl_nlp.framenet.adapter import JSONAdapter
from srl_nlp.framenet.corpus import Annotation, AnnotationSet, Layer, Sentence
from srl_nlp.logger_config import timeit, config_logger, add_logger_args as _add_logger_args
# from framenet.adapter import SemEval07XMLAdapter
from srl_nlp.data_augmentation.logical_representation.fol import FOL
from srl_nlp.data_augmentation.logical_representation.logicalform import LF
from srl_nlp.rule_utils import open_a_file
from srl_nlp.annotation_utils import get_factors

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


class Annotator(object):

    @abstractmethod
    def __init__(self):
        pass

    @abstractmethod
    def matching(self, sentences):
        # type: (List[Sentence]) -> List[Sentence]
        pass


class PrologAPI:

    @abstractmethod
    def __init__(self):
        pass

    @staticmethod
    def load_file(file_name):
        """script method of prolog, loads specific file"""
        cmd = '[\'{f}\'].'.format(f=file_name)
        return cmd

    @staticmethod
    def forall(predicate, arity=0):
        """script method of prolog, lists all groundings of this predicate"""
        var_list = ','.join(['C%d' % i for i in range(arity)])
        cmd = 'forall({pred}({vars}), writeln({pred}({vars}))).'.format(pred=predicate, vars=var_list)
        return cmd

    @staticmethod
    def halt():
        """script method of prolog, end of script"""
        return 'halt.'

    @staticmethod
    def _script(*cmds):
        """script method of prolog, build the script from a sequence of commands"""
        return "\n".join(cmds)


class PrologAnnotator(Process, Annotator):
    """PrologAnnotator runs a Prolog inference over the given sentence that has been processed by the analyser"""
    FRAME_RELATED_PRED = 'frame_related'
    FRAME_ELEMENT_PRED = 'frame_element'

    def __init__(self, analyser, fr_kb_file, fe_kb_file, path_to_prolog=config.get('prolog_local', 'engine')):
        self.analyser = analyser
        self.fr_kb_file = fr_kb_file
        self.fe_kb_file = fe_kb_file
        self._nlp = spacy.load(config.get('syntactic_local', 'spacy_model'))  # TODO make it an argument later
        self._get_lemma = lambda token: self._nlp(token.decode('utf-8'))[0].lemma_
        # Annotator.__init__(self)
        Process.__init__(self, path_to_prolog, True)

    def _process_output(self, out):
        # """Overrides method from Process class"""
        preds = out.strip().split('\n')
        out = []
        for line in preds:
            if len(line) > 0:
                logger.debug('Process Out Line: \'%s\'' % line)
                lf = LF(line)
                logger.debug('Process Out LF:   \'%s\'' % lf)
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

    def store_lf(self, sentence, out_file, footer='', header=''):
        # type: (str,any, str, str) -> None
        lfs = self.analyser.sentence2LF(sentence)
        out_file.write(header)
        for lf in lfs:
            for pred in lf.split():
                if pred.get_pred() != FOL.OR:
                    logger.debug('PRED: {}'.format(str(pred)))
                    out_file.write(str(pred) + '\n')
        out_file.write(footer)
        out_file.flush()
        out_file.seek(0)

    def matching(self, sentence, lf_file_name=None, **params):
        with open_a_file(lf_file_name) as lf_file:
            script = PrologAPI._script(PrologAPI.load_file(self.fr_kb_file),
                                       PrologAPI.load_file(self.fe_kb_file),
                                       PrologAPI.load_file(lf_file.name),
                                       PrologAPI.forall(PrologAnnotator.FRAME_RELATED_PRED, 2),
                                       PrologAPI.forall(PrologAnnotator.FRAME_ELEMENT_PRED, 3),
                                       PrologAPI.halt())
            self.store_lf(sentence, out_file=lf_file)
            out, err = self._process(script)
        return out, err

    def sem_annotations(self, sentence, sentence_lfs=None, anno_lfs=None, var2pos_list=None):
        """Gets the annotations in f

            Args:
                sentence: sentence that is going to be matched
                sentence_lfs: list of logical form representations of the sentence
                anno_lfs: predicate list
                var2pos_list: dictionary mapping variable string to list of tuples (start,end)

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
                            frame = frs.get(frame_name, AnnotationSet(anno_set_id="%07d" % count, frame_name=frame_name,
                                                                      status=self.analyser.name))
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
        # type: (str, List[LF],Dict[any,any]) -> Dict[LF,List[any]]
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


#
# class PrologBoxerAnnotator(Annotator, Process):
#     """PrologAnnotator runs a Prolog inference over the given sentence that has been processed by the analyser"""
#     FRAME_RELATED_PRED = 'frame_related'
#     FRAME_ELEMENT_PRED = 'frame_element'
#
#     def __init__(self, analyser, fr_kb_file=None, fe_kb_file=None, path_to_prolog=config.get('prolog_local', 'engine')):
#         self.analyser = analyser
#         self.fr_kb_file = fr_kb_file
#         self.fe_kb_file = fe_kb_file
#         self._nlp = spacy.load(config.get('syntactic_local', 'spacy_model'))  # TODO make it an argument later
#         self._get_lemma = lambda token: self._nlp(token.decode('utf-8'))[0].lemma_
#         # Annotator.__init__(self)
#         Process.__init__(self, path_to_prolog, True)
#
#     def _process_output(self, out):
#         # """Overrides method from Process class"""
#         preds = out.strip().split('\n')
#         out = []
#         for line in preds:
#             if len(line) > 0:
#                 logger.debug('Process Out Line: \'%s\'' % line)
#                 lf = LF(line)
#                 logger.debug('Process Out LF:   \'%s\'' % lf)
#                 out.append(lf)
#         return out
#
#     def _indexes_token_in_sentence(self, token, sentence):
#         """Finds out if method can be found in the sentence, uses the nlp pipeline to lemmatize
#
#         Returns the indexes of the given token in the sentence.
#         If there is no matching, returns None"""
#         token = token.decode()
#         tokens = self._nlp(sentence.decode())
#         for s_token in tokens:
#             if token == s_token.lemma_:
#                 return s_token.idx + len(s_token)
#         return None
#
#     def _indexes_term_in_sentence(self, link_term, lfs, sentence):
#         factors = [get_factors(lf) for lf in lfs]
#         for term, predicate in factors:
#             if term == link_term:
#                 if predicate.get_pred() in self.analyser.DEFINITION_TERMS:
#                     _, token = predicate.iteritems()
#                     return self._indexes_token_in_sentence(token, sentence)
#         return None
#
#     def store_lf(self, sentence, out_file, footer='', header=''):
#         # type: (str,any, str, str) -> None
#         lfs = self.analyser.sentence2LF(sentence)
#         out_file.write(header)
#         for lf in lfs:
#             for pred in lf.split():
#                 if pred.get_pred() != FOL.OR:
#                     logger.debug('PRED: {}'.format(str(pred)))
#                     out_file.write(str(pred) + '\n')
#         out_file.write(footer)
#         out_file.flush()
#         out_file.seek(0)
#
#     def _frame_matching(self, sentence, lf_file_name=None):
#         # type: (str, str) -> (str, str)
#         """
#
#         Args:
#             sentence: String, the sentence to be annotated with frame information
#             lf_file_name: file to where store results of the annotation
#
#         Returns:
#             If out_error is false, it returns a list of frame_related predicates.
#             If out_error is true, then it returns a tuple with the list of predicates and the error output.
#
#         """
#         with open_a_file(lf_file_name) as lf_file:
#             script = PrologAPI._script(PrologAPI.load_file(self.fr_kb_file),
#                                        PrologAPI.load_file(lf_file.name),
#                                        PrologAPI.forall(PrologBoxerAnnotator.FRAME_RELATED_PRED, 2),
#                                        PrologAPI.halt())
#             self.store_lf(sentence, out_file=lf_file)
#             out, err = self._process(script)
#             return out, err
#
#     def _frame_element_matching(self, sentence, **params):
#         # type: (str, List, str, any) -> Tuple[str,str]
#         with open_a_file(lf_file_name) as lf_file:
#             script = PrologAPI._script(PrologAPI.load_file(self.fe_kb_file),
#                                        PrologAPI.load_file(lf_file.name),
#                                        PrologAPI.forall(PrologBoxerAnnotator.FRAME_ELEMENT_PRED, 3),
#                                        PrologAPI.halt())
#             self.store_lf(sentence, header='\n'.join(map(str, fr_anno)) + '\n', out_file=lf_file)
#             out, err = self._process(script)
#
#             return out, err
#
#     def matching(self, sentences):
#         with open_a_file(lf_file_name) as lf_file:
#             script = PrologAPI._script(PrologAPI.load_file(self.fr_kb_file),
#                                        PrologAPI.load_file(self.fe_kb_file),
#                                        PrologAPI.load_file(lf_file.name),
#                                        PrologAPI.forall(PrologBoxerAnnotator.FRAME_RELATED_PRED, 2),
#                                        PrologAPI.forall(PrologBoxerAnnotator.FRAME_ELEMENT_PRED, 3),
#                                        PrologAPI.halt())
#             self.store_lf(sentences, out_file=lf_file)
#             out, err = self._process(script)
#         return out, err
#
#     def sem_annotations(self, sentences, **kwargs):
#         """Gets the annotations in f
#
#             Args:
#                 sentences: sentence that is going to be matched
#                 **kwargs: Other parameter for compatibility. Ignored.
#
#             Returns:
#                 A dictionary of frames names and annotation sets
#         """
#
#         # TODO change frame_element(Var, FE, Frame) to frame_element(Var, FE, Var_frame)
#         # TODO implement the interval closing
#         # Get dictionaries to perform matching
#         if not anno_lfs:
#             return dict()
#         if not sentence_lfs:
#             sentence_lfs = self.analyser.sentence2LF(sentences)
#         if var2pos_list is None:
#             token2pos = self.analyser.get_matching_tokens(sentences, 'pos')
#             var2pos_list = self.get_matching_variables(sentences, sentence_lfs, token2pos)
#         # Run through lfs to get the info
#         frs = dict()
#         count = 0
#         for lf in anno_lfs:
#             for pred in lf.split():
#                 for child in pred.iterterms():
#                     child_pred = child.get_pred()
#                     if child_pred in var2pos_list:
#                         if pred.get_pred() == 'frame_related':
#                             frame_name = pred.info[-1][0]
#                             frame = frs.get(frame_name, AnnotationSet(anno_set_id="%07d" % count, frame_name=frame_name,
#                                                                       status="Breno"))
#                             layer = Layer(name="Target")
#                             for start, end in var2pos_list[child_pred]:
#                                 layer.annotations.append(Annotation(name="Target", start=start, end=end))
#                             frame.layers.append(layer)
#                             frs[frame_name] = frame
#                             count = count + 1
#                         break
#
#         for lf in anno_lfs:
#             for pred in lf.split():
#                 for child in pred.iterterms():
#                     child_pred = child.get_pred()
#                     if child_pred in var2pos_list:
#                         if pred.get_pred() == 'frame_element':
#                             frame_name = pred.info[3][0]
#                             fe_name = pred.info[2][0]
#                             if frame_name in frs:
#                                 frame = frs[frame_name]
#                                 layers = [layer for layer in frame.layers if layer.name == "FE"]
#                                 if len(layers) > 0:
#                                     layer = layers[0]
#                                 else:
#                                     layer = Layer(name="FE")
#                                     frame.layers.append(layer)
#                                 for start, end in var2pos_list[child_pred]:
#                                     layer.annotations.append(Annotation(start=start, end=end, name=fe_name))
#                             else:
#                                 # TODO better handle this exception
#                                 # raise Exception("Oh fork, this frame element ({fe}) is orphan.".format(fe=fe_name))
#                                 logger.error("Oh fork, this frame element ({fe}) is orphan.".format(fe=fe_name))
#                                 break
#         return frs
#
#     def get_matching_variables(self, sentence, sentence_lfs=None, matching=None):
#         # type: (str, List[LF],Dict[any,any]) -> Dict[LF,List[any]]
#         if not sentence_lfs:
#             sentence_lfs = self.analyser.sentence2LF(sentence)
#         if not matching:
#             matching = self.analyser.get_matching_tokens(sentence, output="pos")
#         matching1 = dict()
#         for lf in sentence_lfs:
#             for pred in lf.split():
#                 for child in pred.iterterms():
#                     if child.get_pred() in matching:
#                         term = pred.iterterms().next().get_pred()
#                         tmp_list = matching1.get(term, [])
#                         tmp_list.append(matching[child.get_pred()])
#                         matching1[term] = tmp_list
#                         break
#         return matching1
#

class SemaforAnnotator(Process, Annotator):

    def __init__(self, path_to_bin=config.get('semantic_local', 'semafor'), time_out=None, num_cpus=8):
        self.parser = JSONAdapter()
        self.in_file_name = NamedTemporaryFile('r+').name  # type: NamedTemporaryFile
        self.out_file_name = NamedTemporaryFile('r+').name
        params = [self.in_file_name, self.out_file_name, str(num_cpus)]
        Process.__init__(self, path_to_bin, True, time_out, *params)

    def matching(self, in_sentences):
        # type: (List[Sentence]) -> List[Sentence]

        in_sentences = [(Sentence(text=str(sent), sent_id=idx)
                         if not isinstance(sent, Sentence) else sent)
                        for idx, sent in enumerate(in_sentences)]  # type: List[Sentence]
        try:
            with open(self.in_file_name, 'w') as in_file:
                for sentence in in_sentences:
                    in_file.write(sentence.text.strip() + '\n')
            out, err = self._process('')
            logger.debug('SEMAFOR ANNOTATOR OUT:\n %s', out)
            if len(err) > 0:
                logger.error('SEMAFOR ANNOTATOR ERR:\n %s', err)
            with open(self.out_file_name, 'r') as out_file:
                out_sentences = [self.parser.parse_sentence(json.loads(str_json_anno))
                                 for str_json_anno in out_file.readlines()]  # type: List[Sentence]

            for in_sent, out_sent in zip(in_sentences, out_sentences):
                out_sent.id = in_sent.id
                out_sent.text = in_sent.text
            logger.debug("Temp out file path: %s", self.out_file_name)
        finally:
            if path.exists(self.in_file_name):
                remove_file(self.in_file_name)
            if path.exists(self.out_file_name):
                remove_file(self.out_file_name)
            logger.debug("SemaforAnnotator: removed temp files")
        return out_sentences

    def __str__(self):
        return 'Semafor Annotator on {}'.format(self.path_to_bin)


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

    logging.basicConfig(level=logging.DEBUG, format="[%(levelname)s:%(name)s:%(filename)s:%(lineno)s] %(message)s")
    semafor = SemaforAnnotator()
    print(semafor.matching([Sentence(sent_id=0, text='this is my testing'),
                            Sentence(sent_id=1, text='life is wonderful.')]))

    # from srl_nlp.analysers.boxer import BoxerLocalAPI
    # boxer = BoxerLocalAPI()
    # kb_fr_path = path.join(args.kb_path, args.kb_fr)
    # kb_fe_path = path.join(args.kb_path, args.kb_fe)
    # anno = PrologBoxerAnnotator(boxer, kb_fr_path, kb_fe_path)

    # print 'LF: %s' % boxer.sentence2LF(args.sentence)
    #
    # if args.frame_matching:
    #     print 'Frame Matching:'  # TODO use different annotators
    #     out, err = anno.matching(args.sentence, lf_file_name=args.tmp_lf_file)
    #     logger.debug(err)
    #     print '\'%s\n\'' % '\n'.join(map(str, out))
    #
    # if args.frame_element_matching:
    #     print '\nFrame Element Matching:'
    #     out, err = anno.frame_element_matching(args.sentence, lf_file_name=args.tmp_lf_file)
    #     logger.debug(err)
    #     print '\'%s\n\'' % '\n'.join(map(str, out))
    #
    # if args.matching:
    #     print '\nMatching:'
    #     out, err = anno.matching(args.sentence, lf_file_name=args.tmp_lf_file)
    #     logger.debug(err)
    #     print '\'%s\n\'' % '\n'.join(map(str, out))
    #     # semeval_parser = SemEval07XMLAdapter()
    #     # for f_name, annoset in anno.sem_annotations(args.sentence, out).items():
    #     #     print "Frame:{f_name}\n\t{anno}".format(f_name=f_name, anno=semeval_parser._anno_set2XML(annoset))

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
