#!/bin/python
'''
Parses the XML files from FrameNet
'''
from sys import argv,stderr
import xml.etree.ElementTree as XMLTree
import json
import logging
import argparse

logger = logging.getLogger(__name__)

class LexicalUnit:
    pass

class Frame:
    pass

class FrameParser:
    '''Parses a XML file describing a Frame into a Frame'''
    def parseXML(file_str):
        return None #Frame()

class LUParser:
    '''Parses a XML file describing a Lexical Unit into a Lexical Unit'''
    def parseXML(file_str):
        return None #LexicalUnit()

class NetParser:
    def __init__(self, fparser, luparser):
        assert isinstance(fparser, FrameParser)
        assert isinstance(luparser, LUParser)
        self._fparser = fparser
        self._luparser = luparser

    def parseXML(str_dir):
        pass

    def parse_framesXML(str_dir):
        pass

    def parse_luXML(str_dir):
        pass


def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Parses the XML files from FrameNet')
    parser.add_argument('dir_path', help = 'the path of the data files')
    parser.add_argument('out_file', help = 'output file name')
    add_logger_args(parser)
    args = parser.parse_args()
    return args

def main(argv):
    args   = parse_args(argv)
    #config_logger(args)
    print 'I am parsing'

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by user')