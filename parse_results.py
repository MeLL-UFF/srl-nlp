#!/bin/python
'''
Parses the ourput of an experiment tree (Aleph only right now)
'''
from os           import path, walk as walkdir
from sys          import argv
from json         import load,dump
from regex        import compile
from learner      import Aleph, run_tree, config_logger
import logging
import argparse

def _runAleph_out_parser(dir, file_list, prefix = None, logger = logging.getLogger(__name__), d = {}):
    file_name = Aleph._find_file(prefix, None, file_list, logger)
    if file_name:
        complete_file_name = path.join(dir, file_name)
        logger.info('File name: %s', complete_file_name)
        d.update({complete_file_name: Aleph.process_out(complete_file_name)})

def parse_args(argv = argv):
    parser = argparse.ArgumentParser(description = 'Parses the ourput of an experiment tree (Aleph only right now)')
    parser.add_argument('dir_path', help = 'the path of the experiments')
    parser.add_argument('-p', '--file_prefix', help = 'prefix of the experiment output files')
    parser.add_argument('-o', '--output_file', help = 'file to store results (if None: print to stdout)')
    parser.add_argument('-v', '--verbosity', action='count', default=0, help = 'increase output verbosity')
    args = parser.parse_args(argv[1:])
    return args

class DictObj:
    '''
    Object representing an experiment output
    '''
    def __init__(self, sep = path.sep, repl = '_', **entries):
        new_entries = {}
        for key in entries.keys():
            tokens = map(lambda x: self._normalize_token(x, repl), key.split(sep))
            if len(tokens) > 1:
                new_entries[tokens[0]] = new_entries.get(tokens[0],DictObj())
                new_entries[tokens[0]]._update(self._unfold(tokens[1:],entries[key]))
            else:
                new_entries[tokens[0]] = entries[key]
        self.__dict__.update(new_entries)

    def _update(self, other):
        for key in other.__dict__:
            if key in self.__dict__:
                if isinstance(self.__dict__[key], DictObj):
                    self.__dict__[key]._update(other.__dict__[key])
            else:
                 self.__dict__[key] = other.__dict__[key]
        

    def _unfold(self, list_tokens, val):
        if len(list_tokens) > 1:
            return DictObj(**{list_tokens[0]: self._unfold(list_tokens[1:], val)})
        else:
            if isinstance(val, dict) or isinstance(DictObj):
                return DictObj(**{list_tokens[0]: DictObj(**val)})
            else:
                return DictObj(**{list_tokens[0]: val})

    def _normalize_token(self, token, repl, forbiden = '+-*, .'):
        for i in forbiden:
            token = token.replace(i, repl)
        return token

def json_to_object(json):
    with open(json, 'r') as f:
        return DictObj(**load(f))


def main(argv):
    args = parse_args(argv)
    logger = config_logger(args.verbosity)
    out = {}
    logger.info('Starting at %s', args.dir_path)
    run_tree(args.dir_path, _runAleph_out_parser, args.file_prefix, logger, d = out)
    if args.output_file:
        with open(args.output_file, 'w') as out_stream:
            dump(out, out_stream)
    else:
        print out
    logger.info('Done')

if __name__ == '__main__':
    logger = logging.getLogger(__name__)
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)