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
        tag_pattern = compile('(?:{0}(.*?){1})|(.+?(?=(?:{0})|^))'.format(open_tag, close_tag), DOTALL)
        in_tag_pattern = compile('\s*((?:\w|-|_)+)\s*(?:name="(\w*)")?\s*', DOTALL)

        tokens      = tag_pattern.findall(xmlNode.text)
        tag_stack   = []

        description = Description(escapeHTML=escapeHTML)
        tag_buffer  = description
        text_buffer = ''
        content     = None
        is_comment  = False
        #print 'SENTENCE:', xmlNode.text
        #print 'TOKENS:', tokens
        for tag, text in tokens:
            #print "tag:'%s', text: '%s'" %(tag, text)
            if len(tag) != 0: #if it is a tag
                name, _ = in_tag_pattern.findall(tag)[0]
                name = name.strip()
                #print 'NAME %s' %name
                if tag.startswith(end_tag_marker):
                    if not is_comment:
                        curr_tag, curr_text = tag_stack.pop()
                        curr_name, attrib = in_tag_pattern.findall(curr_tag)[0]

                        #print 'CURNAME %s, attrib %s, name %s' %(curr_name, attrib, name)
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
                                logger.debug('Weird tag "{name}" from text'.format(name = name, text= xmlNode.text))
                                #raise Exception('{text}\nWeird tag "{tag}"'.format(tag = name, text= xmlNode.text))
                        else:
                            raise Exception('{text}\n{name} != {cname}'.format(name = name, cname =curr_name, text= xmlNode.text))
                else:
                    if name.startswith('--'):
                        is_comment = not is_comment
                    if not is_comment:
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
                logger.debug('%3d reading frame "%s"',
                             ((count+1)*100)/len(files_names),
                             file_name)
                frame = self._fparser.parse(file_path)
                frames[frame.name] = frame

        return Net(frames)

    def parse_frames(self, str_dir):
        pass


def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Parses the XML files from FrameNet')
    parser.add_argument('dir_path', help = 'the path of the data files')
    parser.add_argument('out_file', help = 'output file name')
    add_logger_args(parser)
    args = parser.parse_args()
    return args

def main(argv):
    #args = parse_args(argv)
    #config_logger(args)
    print 'I am parsing'
    f = FrameXMLParser()
    class A(): pass
    a = A()
    definition = '&lt;def-root&gt;&lt;fen&gt;Authorities&lt;/fen&gt; charge a &lt;fen&gt;Suspect&lt;/fen&gt;, who is under suspicion of having committed a crime (the &lt;fen&gt;Charges&lt;/fen&gt;), and take him/her into custody.&lt;ex&gt;&lt;fex name="Authorities"&gt;The police&lt;/fex&gt; &lt;t&gt;arrested&lt;/t&gt; &lt;fex name="Suspect"&gt;Harry&lt;/fex&gt; &lt;fex name="Chrg"&gt;on charges of manslaughter&lt;/fex&gt;.&lt;/ex&gt;&lt;/def-root&gt;'
    setattr(a,'text', definition)
    d = f._parse_description(a, escapeHTML = True)
    assert str(d) == definition
    print 'Matching worked'

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by user')