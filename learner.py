#!/bin/env python
'''
Runs all the experiments in a given directory tree
'''

from os              import path, walk as walkdir
from sys             import argv
from json            import load, dump
from regex           import compile
from subprocess      import PIPE, Popen as popen
from ConfigParser    import ConfigParser
from logger_config   import config_logger, add_logger_args

#from problog import get_evaluatable
from problog.program import PrologFile
from probfoil        import probfoil
from probfoil.data   import DataFile
from probfoil.score  import accuracy, precision, recall

import logging
import argparse
#import probfoil

logger = logging.getLogger(__name__)

config = ConfigParser()
_package_directory = path.dirname(__file__)

config.read(path.join(_package_directory, "external.conf"))

class Learner(object):
    def __init__(self, *args, **kargs):
        self.name = 'Dummy'

    def run_learning(self, out_file = None, **kargs):
        assert True, 'abstract method!'

    def __repr__(self):
        return self.name

    @staticmethod
    def _find_file(prefix, extension, file_list, logger = logging.getLogger(__name__)):
        '''
        Returns the first file with the given prefix and extension.

        Aleph._find_file(str,str,[str,...]) -> str

        Params:
            prefix: the prefix of the file name
            extension: the desired extension of the file
            file_list: the list of files to pick from
        '''
        candidates = map(lambda x: path.splitext(x), file_list)
        if len(candidates) < 1:
            logger.debug('No candidates')
        else:
            if prefix:
                match_prefix = filter(lambda x: x[0].startswith(prefix), candidates)
            else:
                match_prefix = candidates
            if extension:
                match_extension = filter(lambda x: x[1][1:] == extension, match_prefix)
            else:
                match_extension = match_prefix
            out = match_extension
            if len(out) > 0:
                if len(out) > 1:
                    logger.warning('More than one file matching the requisites %s',
                                        ', '.join(out))
                return ''.join(out[0])
            else:
                logger.warning('No file matching requirements: prefix ="%s", ext="%s"',
                                    prefix, extension)
                logger.warning('Candidates: %s...', str(file_list))
        return None

class Aleph(Learner):
    '''
    API to run the Aleph Learning Algorithm
    '''
    def __init__(self, dir, files, *args, **kargs):
        '''Initializes the Aleph API

        Params:
            dir: directory with the experiment data
            files: list of files to be selected for learning based on their extension
            prefix: [optional] consider only files with this prefix
            script: [optional] overwrites the location of the execution script
        '''
        super(Aleph, self).__init__(*args, **kargs)
        script_config = config.get('aleph_local','script')
        self.script = kargs.get('script',script_config)
        self.name   = 'Aleph'
        self.dir    = dir
        self.prefix = kargs.get('prefix', None)
        logger.debug('Checking necessary files in %s', self.dir)
        #self.kb     = Aleph._find_file(self.prefix,'pl', files)
        self.neg    = Aleph._find_file(self.prefix, 'n', files)
        self.fact   = Aleph._find_file(self.prefix, 'f', files)
        self.base   = Aleph._find_file(self.prefix, 'b', files)

    def _has_necessary_files(self):
        '''Returns if the learner has all the files it requires to function
        '''
        return self.neg and self.fact and self.base

    def get_files(self):
        '''
        Returns a list of the relevant files for learning
        '''
        return (self.neg, self.fact, self.base)

    def get_prefix(self):
        '''
        Returns the prefix of the files used for learning
        '''
        if self.prefix:
            return self.prefix
        else:
            prefixes = map(lambda x: path.splitext(x)[0], self.get_files())
            prefixes = filter(lambda x: x, prefixes)
            if len(prefixes) > 1:
                for f in prefixes[1:]:
                    if f != prefixes[0]:
                        e = Exception('There is no unique prefix, %s' %', '.join(self.get_files()))
                        logger.critical(e)
                        raise e
                return prefixes[0]
            else:
                if len(prefixes) > 0:
                    return prefixes[0]
                else:
                    e = Exception('There is no prefix')
                    logger.critical(e)
                    raise e

    def run_learning(self, out_file_name = None, **kargs):
        ''' Calls Yap to run Aleph and process the output

            Params:
                out_file_name: [optional] name of the output file to be generated in the experiment folder
            
            If there is no out_file_name this method is going to print the output
        '''
        to_str = True if out_file_name else False
        if self._has_necessary_files():
            if out_file_name:
                out_stream = open(path.join(self.dir, out_file_name), 'w')
            else:
                out_stream = PIPE
            logger.debug('%s <- [neg=%s, facts=%s, base=%s]',
                              self.name,
                              *self.get_files())
            process_args = [self.script, self.dir, self.get_prefix()]
            logger.debug("%s process args: %s", self.name, ', '.join(process_args))
            process = popen(process_args, stdout = out_stream, stderr = PIPE)
            if out_file_name:
                out_stream.close()
            else:
                for line in process.stdout.readlines():
                    print line,
            err = process.stderr.read()
            if err:
                logger.warning("%s STDERR:\n%s\n%s", self.name.upper(), self.dir, err)
        else:
            logger.debug('Empty dir: %s', self.dir)

    @staticmethod
    def process_out(in_file_name):
        #TODO process_out
        catch_theory = compile('%\s*\[theory\].*')
        catch_theory_end = compile('\s*/\*.*')
        catch_accuracy = compile('\s*Accuracy\s*=\s*(\d*\.\d*)\s*')
        catch_table = compile('\[Training set performance\].*')
        NEUTRAL_MODE, THEO_MODE, TABLE_MODE = range(3)
        mode = NEUTRAL_MODE
        theory = []
        accuracy = 0
        table = []
        with open(in_file_name, 'r') as in_file:
            for line in in_file:
                if mode == NEUTRAL_MODE:
                    found_theo = catch_theory.match(line)
                    if found_theo:
                        mode = THEO_MODE
                        continue
                    found_table = catch_table.match(line)
                    if found_table:
                        mode = TABLE_MODE
                        continue
                elif mode == THEO_MODE:
                    found_end = catch_theory_end.match(line)
                    if found_end:
                        mode = NEUTRAL_MODE
                    else:
                        theory.append(line)
                    continue
                elif mode == TABLE_MODE:
                    found_acc = catch_accuracy.match(line)
                    if found_acc:
                        accuracy = float(found_acc.group(1))
                        mode = NEUTRAL_MODE
                        break
                    else:
                        table.append(line)
        #assert mode == NEUTRAL_MODE
        if mode != NEUTRAL_MODE:
            log.warning("File ended before analysis")
        out = {'table': ''.join(table),
               'accuracy': accuracy,
               'theory': ''.join(theory)}
        logger.debug(out)
        return out

class ProbFoil(Learner):

    def __init__(self, dir, files, *args, **kargs):
        '''Initializes the ProbFoil API

        Params:
            dir: directory with the experiment data
            files: list of files to be selected for learning based on their extension
            prefix: [optional] consider only files with this prefix
        '''
        super(ProbFoil, self).__init__(*args, **kargs)
        self.name     = 'ProbLog'
        self.dir      = dir
        self.prefix   = kargs.get('prefix', None)
        logger.debug('Checking necessary files in %s', self.dir)
        #self.kb       = ProbFoil._find_file(self.prefix,'pl', files)
        self.neg      = ProbFoil._find_file(self.prefix, 'n', files)
        self.fact     = ProbFoil._find_file(self.prefix, 'f', files)
        self.base     = ProbFoil._find_file(self.prefix, 'b', files)


    @staticmethod
    def process_out(in_file_name):
        out = ''
        return out

    def get_files(self):
        '''
        Returns a list of the relevant files for learning
        '''
        return (self.neg, self.fact, self.base)

    def _has_necessary_files(self):
        '''Returns if the learner has all the files it requires to function
        '''
        return True #TODO _has_necessary_files

    def run_learning(self, out_file_name = None, **kargs):
        ''' Runs ProbFoil and process the output

            Params:
                out_file_name: [optional] name of the output file to be generated in the experiment folder
            
            If there is no out_file_name this method is going to print the output
        '''
        to_str = True if out_file_name else False
        if self._has_necessary_files():
            logger.debug('%s <- %s',
                              self.name,
                              self.get_files())

            data = DataFile(*(PrologFile(source) for source in self.get_files()))
            if kargs.get('probfoil1', False):
                learn_class = probfoil.ProbFOIL
            else:
                learn_class = probfoil.ProbFOIL2

            results = {}
            try:
                learner      = learn_class(data)
                hypothesis   = learner.learn()
                rules        = hypothesis.to_clauses(hypothesis.target.functor)
                skiped_rules = 1 if len(rules) > 1 else 0

                results['rules']     = [str(r) for r in rules[skiped_rules:]]
                results['accuracy']  = accuracy(hypothesis)
                results['precision'] = precision(hypothesis)
                results['recall']    = recall(hypothesis)
            except Exception as e:
                logger.exception(e)

            if out_file_name:
                    with open(path.join(self.dir, out_file_name), 'w') as out_stream:
                        dump(results, outstream)
            else:
                print result
        else:
            logger.debug('Empty dir: %s', self.dir)

def run_tree(dir, func, prefix = None, **kargs):
    _, subdir, files = walkdir(dir).next()
    #logger.debug('Running on %s', dir)
    if len(files) > 0:
        logger.debug("Files: %s", ', '.join(files))
    if len(subdir) > 0:
        for child in subdir:
            run_tree(path.join(dir,child), func, prefix, logger, **kargs)
    func(dir, files, prefix, logger, **kargs)

def _runAleph(dir, file_list, prefix = None):
    learner = Aleph(dir, file_list, prefix = prefix)
    learner.run_learning('out.txt')

def _runProbFoil(dir, file_list, prefix = None):
    learner = ProbFoil(dir, file_list, prefix = prefix)
    learner.run_learning('out.txt')

def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Runs the experiments defined in each folder (Aleph only right now)')
    parser.add_argument('dir_path', help = 'the path of the experiments')
    parser.add_argument('-p', '--file_prefix', help = 'prefix of the experiment files')
    #parser.add_argument('-v', '--verbosity', action='count', default=0, help = 'increase output verbosity')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args

def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    logger.info('Starting at %s', args.dir_path)
    run_tree(args.dir_path, _runAleph, args.file_prefix)
    logger.info('Done')

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)