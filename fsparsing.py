#!/bin/env python
'''
Description
'''#TODO description

from srl_nlp.analysers.process import Process
from srl_nlp.rule_manipulation import *
from srl_nlp.logicalform       import LF
from logger_config             import config_logger, add_logger_args
from ConfigParser              import ConfigParser
from tempfile                  import NamedTemporaryFile
from os                        import path
from sys                       import argv
import logging
import argparse
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
    '''Description of annotator 1'''
    FRAME_RELATED_PRED = 'frame_related'
    FRAME_ELEMENT_PRED = 'frame_element'

    def __init__(self, analyser, fr_kb_file, fe_kb_file, path_to_prolog = config.get('prolog_local','engine')):
        self.analyser = analyser
        self.fr_kb_file = fr_kb_file
        self.fe_kb_file = fe_kb_file
        SemanticAnnotator.__init__(self)
        Process.__init__(self, path_to_prolog, True)

    def _load_file(self, file_name):
        cmd = '[\'{f}\'].'.format(f = file_name)
        return cmd

    def _forall(self, predicate, arity = 0):
        vars = ','.join(['C%d' %i for i in range(arity)])
        cmd = 'forall({pred}({vars}), writeln({pred}({vars}))).'.format(pred = predicate, vars = vars)
        return cmd

    def _halt(self):
        return 'halt.'

    def _script(self , *cmds):
        return "\n".join(cmds)

    def _open_a_file(self, name = None):
        'Opens the file, if no name is given, opens a NamedTemporaryFile'
        if name != None:
            return open(name, 'wr')
        else:
            return NamedTemporaryFile()

    def _process_output(self, out):
        preds = out.strip().split('\n')
        out = []
        for line in preds:
            if len(line) > 0:
                logger.debug('ProcessOutLine: \'%s\'' %line)
                lf = LF(line)
                logger.debug('ProcessOutLF:   \'%s\'' %lf)
                out.append(lf)
        return out

    #TODO
    def _preds2example(self, sentence, script_out):
        lines = []
        for line in script_out.split('\n'):
            # TODO handle comments
            lines.append(line.strip)
        lfs =  [LF(line) for line in lines]
        for lf in lfs:
            # TODO if lf is a frame related, store the frame, find the token linked with the term and try to match it in the sentence
            if lf.get_pred() == Annotator1.FRAME_RELATED_PRED:
                pass

            # TODO if lf is a frame element related, store the fe, find the token linked with the term and try to match it in the sentence
            if lf.get_pred() == Annotator1.FRAME_ELEMENT_PRED:
                pass
        example = None
        return example

    def parsing(self, script, sentence, header = '', footer = '', input_file = None):
        lfs = self.analyser.sentence2LF(sentence)
        lf = lfs[0]
        input_file.write(header)
        for pred in lf.iterterms():
            logger.debug('PRED: {}'.format(str(pred)))
            input_file.write(str(pred)+'\n')
        input_file.write(footer)
        input_file.flush()
        input_file.seek(0)
        logger.debug('\n"{}"\n'.format(script))
        out, err = self._process(script)
        return out, err


    def frameMatching(self, sentence, out_error = False, lf_file_name = None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_RELATED_PRED, 2),
                                  self._halt())
            out, err = self.parsing(self, script, sentence, header = '', input_file = lf_file)
            if out_error:
                return out, err
            else:
                return out

    def frameElementMatching(self, sentence, fr_anno=[], out_error = False,
                             lf_file_name = None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_ELEMENT_PRED, 2),
                                  self._halt())
            out, err = self.parsing(self, script, sentence, header = '\n'.join(fr_anno) + '\n', input_file = lf_file)
            if out_error:
                return out, err
            else:
                return out

    def matching(self, sentence, out_error = False, lf_file_name = None, **params):
        with self._open_a_file(lf_file_name) as lf_file:
            script = self._script(self._load_file(self.fr_kb_file),
                                  self._load_file(self.fe_kb_file),
                                  self._load_file(lf_file.name),
                                  self._forall(Annotator1.FRAME_RELATED_PRED, 2),
                                  self._forall(Annotator1.FRAME_ELEMENT_PRED, 2),
                                  self._halt())
            out, err = self.parsing(self, script, sentence, input_file = lf_file)
            if out_error:
                return out, err
            else:
                return out


def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Runs the experiments defined in each folder (Aleph only right now)')
    #parser.add_argument('dir_path', help = 'the path of the experiments')
    parser.add_argument('sentence',
                        help = 'the sentence to be matched')
    parser.add_argument('-t','--tmp_lf_file',
                        help = 'save lf generated for inspection')
    parser.add_argument('-f','--frame_matching',
                        action='store_true', default=False,
                        help = 'show the frame matching process')
    parser.add_argument('-e','--frame_element_matching',
                        action='store_true', default=False,
                        help = 'show the frame element matching process')
    parser.add_argument('-m','--matching',
                        action='store_true', default=False,
                        help = 'show the matching of both')

    #parser.add_argument('-i', '--stdin', action='store_true', default=False, help = 'increase output verbosity')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args

def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    logger.info('Starting')

    from srl_nlp.analysers.boxer import BoxerLocalAPI
    boxer = BoxerLocalAPI()
    anno = Annotator1(boxer, 'tmp_rules_kb_fr', 'tmp_rules_kb_fe')

    if args.frame_matching:
        print 'Frame Matching:'
        out, err = anno.frameMatching(args.sentence, out_error = True, lf_file_name = args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' % '\n'.join(map(str, out))

    if args.frame_element_matching:
        print '\nFrame Element Matching:'
        out, err = anno.frameElementMatching(args.sentence, out_error = True, lf_file_name = args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' %'\n'.join(map(str, out))

    if args.matching:
        print '\nMatching:'
        out, err = anno.matching(args.sentence, out_error = True, lf_file_name = args.tmp_lf_file)
        logger.debug(err)
        print '\'%s\n\'' %'\n'.join(map(str, out))

    logger.info('Done')

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)