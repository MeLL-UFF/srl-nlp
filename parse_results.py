#!/bin/python
'''
Parses the ourput of an experiment tree (Aleph only right now)
'''
from os           import path, walk as walkdir
from sys          import argv
from json         import load
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


def main(argv):
    args = parse_args(argv)
    logger = config_logger(args.verbosity)
    out = {}
    logger.info('Starting at %s', args.dir_path)
    run_tree(args.dir_path, _runAleph_out_parser, args.file_prefix, logger, d = out)
    if args.output_file:
        with open(args.output_file, 'w') as out_stream:
            json.dump(out, out_stream)
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