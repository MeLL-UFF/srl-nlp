#!/bin/python
'''
Parses the XML files from FrameNet
'''
from sys                     import argv,stderr
from os                      import path, listdir as ls
from re                      import compile, DOTALL
from framenet                import *
from srl_nlp.logger_config   import config_logger, add_logger_args
import xml.etree.ElementTree as XMLTree
import json
import logging
import argparse

logger = logging.getLogger(__name__)


class FrameXMLParser:
    '''Parses a XML file describing a Frame into an actual Frame
       FrameNet version: 1.7'''

    def __init__(self, **args):
        pass

    def _parse_fe(self, xmlNode):
        attrib = xmlNode.attrib
        definition = None
        for grandchild in xmlNode:
            if grandchild.tag.endswith('definition'):
                definition = self._parse_description(grandchild)
        fe = Frame.Element(name         = attrib['name'],
                           abbrev       = attrib['abbrev'],
                           definition   = definition,
                           fgColor      = attrib['fgColor'],
                           bgColor      = attrib['bgColor'],
                           isCore       = attrib.get('coreType') == 'Core',
                           semanticType = attrib.get('semanticType',''))
        return fe

    def _parse_description(self, xmlNode, open_tag = '<', close_tag = '>',end_tag_marker = '/', escapeHTML = False):
        comment_pattern = compile('{open}!--.*?--{close}'.format(open = open_tag, close = close_tag), DOTALL)
        tag_pattern = compile('(?:{open}(.*?){close})|(.+?(?=(?:{open})|^))'.format(open = open_tag, close = close_tag), DOTALL)
        in_tag_pattern = compile('\s*((?:\w|-|_)+)\s*(?:name="(\w*)")?\s*', DOTALL)

        text        = comment_pattern.sub('', xmlNode.text)# if xmlNode.text != None else ''
        tokens      = tag_pattern.findall(text)
        tag_stack   = []

        description = Description(escapeHTML=escapeHTML)
        tag_buffer  = description
        text_buffer = ''
        content     = None

        logger.debug('SENTENCE:%s', xmlNode.text)
        logger.debug('TOKENS:%s', tokens)

        for tag, text in tokens:
            #print "tag:'%s', text: '%s'" %(tag, text)
            if len(tag) != 0: #if it is a tag
                name, _ = in_tag_pattern.findall(tag)[0]
                name = name.strip()
                if tag.startswith(end_tag_marker):
                    curr_tag, curr_text = tag_stack.pop()
                    curr_name, attrib = in_tag_pattern.findall(curr_tag)[0]

                    logger.debug('Last_tag \'%s\', attrib \'%s\', tag \'%s\'', curr_name, attrib, name)
                    if name == curr_name:
                        #print 'name is matching!!'
                        if name == 'ex':
                            if content != None:
                                tag_buffer.add_text(content)
                            description.add_element(tag_buffer)
                            tag_buffer = description
                        elif name == 'fex':
                            tag_buffer.add_element(Description.FEeXample(attrib, content, escapeHTML=escapeHTML))
                        elif name == 'fen':
                            tag_buffer.add_element(Description.FEName(content, escapeHTML=escapeHTML))
                        elif name == 't':
                            tag_buffer.add_element(Description.T(content, escapeHTML=escapeHTML))
                        elif name == 'm':
                            tag_buffer.add_element(Description.M(content, escapeHTML=escapeHTML))
                        elif name == 'ment':
                            tag_buffer.add_element(Description.Ment(content, escapeHTML=escapeHTML))
                        elif name == 'gov':
                            tag_buffer.add_element(Description.Gov(content, escapeHTML=escapeHTML))
                        elif name == 'em':
                            tag_buffer.add_element(Description.EM(content, escapeHTML=escapeHTML))
                        elif name == 'supp':
                            tag_buffer.add_element(Description.Supp(content, escapeHTML=escapeHTML))
                        elif name == 'target':
                            tag_buffer.add_element(Description.Target(content, escapeHTML=escapeHTML))
                        elif name == 'def-root':
                            pass
                        elif name == 'x':
                            pass
                        else:
                            logger.warning('Unknown tag "{tag}"'.format(tag = name))
                            #raise Exception('{text}\nWeird tag "{tag}"'.format(tag = name, text= xmlNode.text))
                    else:
                        raise Exception('Tag is not properly closed: {text}\n{name} != {cname}'.format(name = name, cname =curr_name, text= xmlNode.text))
                else:
                    tag_stack.append((tag, text))
                    if content != None:
                        tag_buffer.add_text(content)
                    if name == 'ex':
                        tag_buffer = Description.EXample(escapeHTML=escapeHTML)
                    if tag.endswith(end_tag_marker):
                        tag_stack.pop()
                content = None
            else:
                content = text.encode('utf-8')
        if len(tag_stack) > 0:
            logger.warning('Not all tags closed!{stack}'.format(stack = tokens))
        return description

    def _parse_lexeme(self, xmlNode):
        attrib = xmlNode.attrib
        text = xmlNode.text if xmlNode.text != None else ''
        le = Lexeme(name        = attrib['name'],
                    pos         = attrib['POS'],
                    breakBefore = attrib['breakBefore'],
                    headWord    = attrib['headword'],
                    text        = text)
        return le

    def _parse_lu(self, xmlNode):
        attrib = xmlNode.attrib
        definition = ''
        lexeme = None
        for grandchild in xmlNode:
            if grandchild.tag.endswith('definition'):
                definition = grandchild.text
            elif grandchild.tag.endswith('sentenceCount'):
                pass
            elif grandchild.tag.endswith('lexeme'):
                lexeme = self._parse_lexeme(grandchild)
                
        lu = LexicalUnit(name       = attrib['name'],
                         pos        = attrib['POS'],
                         status     = attrib['status'],
                         definition = definition,
                         #anottation = (0,0),
                         lexeme     = lexeme)
        return lu

    def parse(self, file_str):
        tree = XMLTree.parse(file_str).getroot()
        #print tree.attrib, file_str
        name = tree.attrib['name']
        coreFEs = []
        peripheralFEs = []
        LUs = []
        definition = None
        relations = dict()
        
        for child in tree:
            if child.tag.endswith('FE'):
                fe = self._parse_fe(child)
                if fe.isCore:
                    coreFEs.append(fe)
                else:
                    peripheralFEs.append(fe)

            elif child.tag.endswith('frameRelation'):
                #TODO
                rel_type = child.attrib['type']
                relations[rel_type] = Frame.Relation(rel_type)

            elif child.tag.endswith('lexUnit'):
                LUs.append(self._parse_lu(child))

            elif child.tag.endswith('definition'):
                definition = self._parse_description(child)
                #print 'TEXT:', definition#child.text

        return Frame(name = name, description = definition, coreFEs = coreFEs,
                 peripheralFEs = peripheralFEs, LUs = LUs, **relations)


class NetXMLParser:
    def __init__(self, frameParser = FrameXMLParser()):
        #assert isinstance(fparser, FrameXMLParser)
        #assert isinstance(luparser, LuXMLParser)
        self._fparser = frameParser

    def parse(self, str_dir):
        frames_path = path.join(str_dir, 'frame')
        frames = dict()
        files_names = ls(frames_path)
        for count, file_name in enumerate(ls(frames_path)):
            if file_name.endswith('.xml'):
                file_path = path.join(frames_path, file_name)
                logger.info('{percent:3d}% reading frame "{absolute}"'.format(percent  = ((count+1)*100)/len(files_names),
                                                                              absolute = file_name))
                frame = self._fparser.parse(file_path)
                frames[frame.name] = frame

        return Net(frames)

    def parse_frames(self, str_dir):
        pass


def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Parses the XML files from FrameNet')
    #parser.add_argument('dir_path', help = 'the path of the data files')
    #parser.add_argument('out_file', help = 'output file name')
    add_logger_args(parser)
    args = parser.parse_args()
    return args

def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    print 'I am parsing'
    parser = NetXMLParser()
    fn = parser.parse('fndata-1.7')
    print '#Frames in FrameNet:', len(fn)
    parser1 = FrameXMLParser()
    class A(): pass
    a = A()
    definition = '<def-root><fen>Authorities</fen> charge a <fen>Suspect</fen>, who is under suspicion of having committed a crime (the <fen>Charges</fen>), and take him/her into custody.<ex><fex name="Authorities">The police</fex> <t>arrested</t> <fex name="Suspect">Harry</fex> <fex name="Chrg">on charges of manslaughter</fex>.</ex></def-root>'
    setattr(a,'text', definition)
    d = parser1._parse_description(a, escapeHTML = False)
    assert str(d) == definition
    print 'Matching worked'

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by user')