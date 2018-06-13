#!/bin/env python2

"""
This module is designed to generate or update all the experiments based on the current state of the 'process_docs.py' module.
"""
import argparse
import json
import logging
from os import path, makedirs, symlink
from random import shuffle
from shutil import copyfile
from sys import argv as _argv

from logger_config import config_logger, add_logger_args
from regex import compile

logger = logging.getLogger(__name__)


class ProcessFile(object):
    def __init__(self, source_file_path, *args, **kargs):
        self.lines = []
        self.source_path = source_file_path  # TODO
        pass

    def dump(self, out_file):
        """Dumps the processed file to out_file"""
        logger.info('Writing to file "%s"', out_file.name)
        out_file.write('\n'.join(self.lines))
        # logger.info('Wrote')


class Aleph:
    class ProcessNegatives(ProcessFile):
        def __init__(self, source_file_path, *args, **kwargs):
            """
            Process the negative files (*.n)
            Params:
                patttern: regex pattern to parse a line
                target:   target predicate to look
            """
            super(Aleph.ProcessNegatives, self).__init__(source_file_path, *args, **kwargs)
            target = kwargs.get('target', 'answer')
            pat = compile(kwargs.get('pattern', '%s\s*\(\s*(\d+)\s*,\s*[^\)]+\s*\)\s*.\s*' % target))
            logger.info('Reading from "%s"', source_file_path)
            with open(source_file_path, 'r') as source:
                dic = {}
                for line in source:
                    match = pat.match(line)
                    if match:
                        l = dic.get(match.group(1), [])
                        l.append(line.strip())
                        if dic.get('random', False):
                            shuffle(l)
                        dic[match.group(1)] = l
                        logger.debug('Negatives %s...%s', str(l)[:50], str(l)[-20:])
                    else:
                        logger.warning('Skipping line: "%s"', line.strip())
            logger.info('Read "%s"', source_file_path)
            lim = kwargs.get('max', None)
            for key in dic:
                if lim is None:
                    self.lines.extend(dic[key])
                else:
                    self.lines.extend(dic[key][:lim])
            logger.info('Storing negatives')

    class ProcessBase(ProcessFile):
        def __init__(self, source_file_path, *args, **kargs):
            """
            Process the negative files (*.n)
            Params:
                patttern: regex pattern to parse a line
                target:   target predicate to look
            """
            super(Aleph.ProcessBase, self).__init__(source_file_path, *args, **kargs)

            logger.info('Reading from "%s"', source_file_path)
            sources = kargs.get('sources', [])
            sources = map(lambda x: x.lower(), sources)
            with open(source_file_path, 'r') as source:
                boxer_mode_lines = []
                boxer_det_lines = []
                dtree_mode_lines = []
                dtree_det_lines = []
                header_lines = []
                footer_lines = []
                setting_dict = {}
                boxer_flag = False
                deptree_flag = False
                header_flag = True
                find_boxer = compile(r'%\s*((?:b|B)oxer)\s*.*')
                find_dtree = compile(r'%\s*((?:d|D)ep(?:t|T)ree)\s*.*')
                find_pred = compile('^\s*:-\s*([^\(]+)\s*\(.*')
                find_set = compile('^\s*:-\s*set\s*\((\w+)\s*,\s*(\w*)\s*\)\s*\.\s*')
                for line in source:
                    pred_match = find_pred.match(line)
                    if find_boxer.match(line):
                        boxer_flag = True
                        deptree_flag = False
                        header_flag = False
                        logger.debug('Reading Boxer info from base')
                    elif find_dtree.match(line):
                        boxer_flag = False
                        deptree_flag = True
                        header_flag = False
                        logger.debug('Reading DepTree info from base')
                    elif pred_match:
                        logger.debug('Line is a predicate: %s', line.strip())
                        if pred_match.group(1) == 'set':
                            # handle_set
                            term, value = find_set.match(line).groups()
                            logger.debug('Setting %s:%s', term, value)
                            setting_dict[term] = value
                        else:
                            if boxer_flag:
                                if pred_match.group(1) == 'modeb':
                                    boxer_mode_lines.append(line.strip())
                                elif pred_match.group(1) == 'determination':
                                    boxer_det_lines.append(line.strip())
                            elif deptree_flag:
                                if pred_match.group(1) == 'modeb':
                                    dtree_mode_lines.append(line.strip())
                                elif pred_match.group(1) == 'determination':
                                    dtree_det_lines.append(line.strip())
                            elif header_flag:
                                header_lines.append(line.strip())
                            else:
                                footer_lines.append(line.strip())
                setting_dict.update(kargs.get('set', {}))
                logger.debug('Settings: %s', setting_dict)
                settings = map(lambda x: ':- set(%s,%s).' % x, setting_dict.iteritems())
                self.lines.append('%Header\n')
                self.lines.extend(header_lines)
                if 'boxer' in sources:
                    self.lines.append('%%%%%%%%%')
                    self.lines.append('% Mode  %')
                    self.lines.append('% Boxer %')
                    self.lines.append('%%%%%%%%%\n')
                    self.lines.extend(boxer_mode_lines)
                if 'deptree' in sources:
                    self.lines.append('%%%%%%%%%%%')
                    self.lines.append('%  Mode   %')
                    self.lines.append('% DepTree %')
                    self.lines.append('%%%%%%%%%%%\n')
                    self.lines.extend(dtree_mode_lines)
                if 'boxer' in sources:
                    self.lines.append('%%%%%%%%%%%%%%%%%')
                    self.lines.append('% Determination %')
                    self.lines.append('%     Boxer     %')
                    self.lines.append('%%%%%%%%%%%%%%%%%\n')
                    self.lines.extend(boxer_det_lines)
                if 'deptree' in sources:
                    self.lines.append('%%%%%%%%%%%%%%%%%')
                    self.lines.append('% Determination %')
                    self.lines.append('%    DepTree    %')
                    self.lines.append('%%%%%%%%%%%%%%%%%\n')
                    self.lines.extend(dtree_det_lines)
                kb = kargs.get('kb', None)
                if kb:
                    self.lines.append(":- consult('%s')." % kb)
                self.lines.extend(footer_lines)
                self.lines.append('%%%%%%%%%%%%')
                self.lines.append('% Settings %')
                self.lines.append('%%%%%%%%%%%%\n')

                self.lines.extend(settings)

    class ProcessFacts(ProcessFile):
        def __init__(self, source_file_path, *args, **kargs):
            """
            Process the fact files (*.f)
            """
            super(Aleph.ProcessFacts, self).__init__(source_file_path, *args, **kargs)
            with open(source_file_path, 'r') as source:
                for line in source:
                    self.lines.append(line.strip())


class ProbLog:
    """
        Notice that some methods inherit from the Aleph class
    """

    class ProcessNegatives(Aleph.ProcessNegatives):
        def __init__(self, source_file_path, *args, **kwargs):
            """
            Process the negative files (*.n)
            Params:
                patttern: regex pattern to parse a line
                target:   target predicate to look
            """
            if not kwargs.has_key('pattern'):
                target = kwargs.get('target', 'answer')
                kwargs['pattern'] = '\d*\.?\d*\s*::\s*%s\s*\(\s*(\d+)\s*,\s*[^\)]+\s*\)\s*.\s*' % target
            super(Aleph.ProcessNegatives, self).__init__(source_file_path, *args, **kwargs)

    class ProcessBase(ProcessFile):
        def __init__(self, source_file_path, *args, **kargs):
            """
            Process the negative files (*.n)
            Params:
                patttern: regex pattern to parse a line
                target:   target predicate to look
            """
            super(ProcessFile, self).__init__(source_file_path, *args, **kargs)
            with open(source_file_path, 'r') as source:
                for line in source:
                    self.lines.append(line.strip())

    class ProcessFacts(ProcessFile):
        def __init__(self, source_file_path, *args, **kargs):
            """
            Process the fact files (*.f)
            """
            super(ProcessFile, self).__init__(source_file_path, *args, **kargs)
            with open(source_file_path, 'r') as source:
                for line in source:
                    self.lines.append(line.strip())


def ensure_dir(dir_name):
    """
    Makes dir if there is no dir
    
        Returns if dir existed before this command
    """
    if not path.exists(dir_name):
        makedirs(dir_name)
        return False
    return True


def write_exp(conf, kb, base, targets, negatives, folder_path, engine, copy_kb=False, prefix='exp', dic=None):
    """
        Creates a hierarchy of folders rooted in folder_path.

        Params:
            conf: a list of parameters (each one as a dictionary)
            kb: the knowledge base file (*.pl)
            base: the base file (*.b)
            targets: the fact file (*.f)
            negatives: the negative file (*.n)
            folder_path: an existing folder that will root the experiments
            copy_kb: copy the knowledge base file to each leaf
                     (only do this if the kb is small)
            prefix: prefix of the leaf files generated
            dic: [internal use]

        The leaf directories all have the files for the particular setting of
        the experiment, plus a short settings.json file indicating the settings
        of this particular experiment.
    """
    if dic == None:
        dic = {}
    if len(conf) == 0:
        file_path = lambda x, y: path.join(folder_path, '%s.%s' % (x, y))
        pN = engine.ProcessNegatives(negatives, **dic.get('n', {}))
        pB = engine.ProcessBase(base, kb='%s.pl' % prefix, **dic.get('b', {}))
        pF = engine.ProcessFacts(targets, **dic.get('f', {}))
        with open(file_path(prefix, 'n'), 'w') as n_file:
            pN.dump(n_file)
        with open(file_path(prefix, 'b'), 'w') as b_file:
            pB.dump(b_file)  # ...
        with open(file_path(prefix, 'f'), 'w') as f_file:
            pF.dump(f_file)
        with open(file_path('settings', 'json'), 'w') as f:
            json.dump(dic, f)
        if copy_kb:
            logger.debug('Copying "%s" to "%s"', kb, file_path(prefix, 'pl'))
            copyfile(kb, path.join(folder_path, file_path(prefix, 'pl')))
        else:
            relpath = path.relpath(kb, folder_path)
            logger.debug('Linking "%s" to "%s"', kb, file_path(prefix, 'pl'))
            symlink(relpath, file_path(prefix, 'pl'))
    else:
        for param in conf[0]:
            d = {}
            for i in ('b', 'n', 'f'):
                d[i] = {}
                d[i].update(dic.get(i, {}))
                d[i].update(conf[0][param].get(i, {}))
            param_path = path.join(folder_path, param)
            ensure_dir(param_path)
            write_exp(conf[1:], kb, base, targets, negatives, param_path, engine,
                      copy_kb, prefix, d)


def parse_args(argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(
        description='Generates or updates all the experiments based on the current state of knowledge base')
    parser.add_argument('kb', help='the knowledge base file')
    parser.add_argument('base', help='the base file')
    parser.add_argument('negatives', help='the negative examples file')
    parser.add_argument('facts', help='the target examples file')
    parser.add_argument('dir_name', help='name of the experiments folder')
    # parser.add_argument('-s', '--settings', help = 'the settings file')
    parser.add_argument('-c', '--copy_kb', action='store_true',
                        help='copy the kb file instead of linking')
    parser.add_argument('-conf', '--configuration_file',
                        help='the configuration file for this particular series of tests')
    # parser.add_argument('-al', '--aleph', help = 'Suppress default Aleph location')
    # parser.add_argument('-probl', '--problog', help = 'Suppress default ProbLog location')
    parser.add_argument('-e', '--engine', choices=['aleph', 'problog'],
                        default='aleph', help='engine')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)

    # Read config file in conf
    if not args.configuration_file:
        args.configuration_file = 'tests.conf'
    with open(args.configuration_file, 'r') as f:
        conf = json.load(f)
    if args.engine == 'aleph':
        engine = Aleph()
    else:
        engine = ProbLog()

    # if not dir_name dir make it
    # if not engine dir make it
    ensure_dir(args.dir_name)
    engine_path = path.join(args.dir_name, args.engine)
    ensure_dir(engine_path)

    write_exp(conf, args.kb, args.base, args.facts, args.negatives,
              engine_path, engine, args.copy_kb)


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
