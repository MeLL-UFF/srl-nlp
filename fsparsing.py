#!/bin/env python
"""
Script for running the Frame Semantic Parsing given some base knowledge
"""

import argparse
from ConfigParser import ConfigParser
from os import path
from sys import argv
from tempfile import NamedTemporaryFile

from logger_config import config_logger, add_logger_args
from srl_nlp.analysers.process import Process
from srl_nlp.framenet.corpus import Sentence, Annotation, AnnotationSet, Layer
from srl_nlp.rule_manipulation import *

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))


class SemanticAnnotator(object):

    def __init__(self, **params):
        pass

    def frameMatching(self, sentence, **params):
        pass

    def frameElementMatching(self, sentence, annotations, **params):
        pass

    def matching(self, sentence, **params):
        pass


class Annotator1(SemanticAnnotator, Process):
    """Description of annotator 1"""
    FRAME_RELATED_PRED = 'frame_related'
    FRAME_ELEMENT_PRED = 'frame_element'

    def __init__(self, analyser, fr_kb_file, fe_kb_file, path_to_prolog=config.get('prolog_local', 'engine')):
        self.analyser = analyser
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
            if inf_lf.get_pred() == Annotator1.FRAME_RELATED_PRED:
                try:
                    link_term, frame_name, fe_name = inf_lf.iterterms()
                    assert link_term.isleaf() and frame_name.isleaf() and fe_name.isleaf()
                except ValueError as e:  # Not correct number of predicates
                    logger.error(e)
                    logger.error('Invalid frame element matching \'{}\''.format(inf_lf))
                    raise e
                token_index = self._indexes_term_in_sentence(link_term, lfs, sentence)
                if token_index:
                    start, end = token_index
                    anno = Annotation(start=start, end=end, name=fe_name)
                    layer = Layer(name='FE', annotations=[anno])
                    f_set_map.setdefault(frame_name,
                                         AnnotationSet(0, frameName=frame_name, status='auto')).layers.append(layer)

                    # logger.error('Token \'{token}\' not found in sentence \'{sent}\''.format(sent = sentence, token = token))

            if inf_lf.get_pred() == Annotator1.FRAME_ELEMENT_PRED:
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
                                         AnnotationSet(0, frameName=frame_name, status='auto')).layers.append(layer)

        return Sentence(id=0, text=sentence, annotation_sets=list(f_set_map.items()))

    def parsing(self, script, sentence, header='', footer='', input_file=None):
        lfs = self.analyser.sentence2LF(sentence)
        lf = lfs[0]
        input_file.write(header)
        for pred in lf.iterterms():
            logger.debug('PRED: {}'.format(str(pred)))
            input_file.write(str(pred) + '\n')
        input_file.write(footer)
        input_file.flush()
        input_file.seek(0)
        logger.debug('\n"{}"\n'.format(script))
        out, err = self._process(script)
        return out, err

    def frameMatching(self, sentence, out_error=False, lf_file_name=None):
        """

        Args:
            sentence: the sentence to be annotated with frame information
            out_error: If true, returns tuple (output, error), else it returns only the output
            lf_file_name: file to where store results of the annotation

        Returns:
            If out_error is false, it returns a list of frame_related predicates.
            If_error is true, then it returns a tuple with the list of predicates and the error output.

        """
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_RELATED_PRED, 2),
                                  self._halt())
            out, err = self.parsing(script, sentence, header='', input_file=lf_file)
            if out_error:
                return out, err
            else:
                return out

    def frameElementMatching(self, sentence, fr_anno=tuple(), out_error=False,
                             lf_file_name=None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_ELEMENT_PRED, 2),
                                  self._halt())
            out, err = self.parsing(script, sentence, header='\n'.join(fr_anno) + '\n', input_file=lf_file)
            if out_error:
                return out, err
            else:
                return out

    def matching(self, sentence, out_error=False, lf_file_name=None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_RELATED_PRED, 2),
                                  self._forall(Annotator1.FRAME_ELEMENT_PRED, 2),
                                  self._halt())
            out, err = self.parsing(script, sentence, input_file=lf_file)
            if out_error:
                return out, err
            else:
                return out


def parse_args(argv=argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='Runs the Boxer analysis and then the frame parsing on the sentence')
    # parser.add_argument('dir_path', help = 'the path of the experiments')

    parser.add_argument('sentence',
                        help='the sentence to be matched')
    parser.add_argument('-t', '--tmp_lf_file',
                        help='save lf generated, for inspection')
    parser.add_argument('-f', '--frame_matching',
                        action='store_true', default=False,
                        help='show the frame matching process')
    parser.add_argument('-e', '--frame_element_matching',
                        action='store_true', default=False,
                        help='show the frame element matching process')
    parser.add_argument('-m', '--matching',
                        action='store_true', default=False,
                        help='show the matching of both')

    parser.add_argument('--kb_fe', default='tmp_rules_kb_fe',
                        help='path to frame element knowledge base')
    parser.add_argument('--kb_fr', default='tmp_rules_kb_fr',
                        help='path to frame matching knowledge base')
    # parser.add_argument('-i', '--stdin', action='store_true', default=False, help = 'increase output verbosity')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    logger.info('Starting')

    from srl_nlp.analysers.boxer import BoxerLocalAPI
    boxer = BoxerLocalAPI()
    anno = Annotator1(boxer, args.kb_fr, args.kb_fe)

    print 'LF: %s\s' % boxer.sentence2LF(args.sentence)

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
        out, err = anno.matching(args.sentence, out_error=True, lf_file_name=args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' % '\n'.join(map(str, out))

    logger.info('Done')


if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.critical(e)
        raise e
