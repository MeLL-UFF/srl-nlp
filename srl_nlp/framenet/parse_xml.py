"""
Parses the XML files from FrameNet
"""

import logging
import xml.etree.ElementTree as XMLTree
from os import path, listdir as ls
from re import compile, DOTALL

from srl_nlp.framenet import description
from srl_nlp.framenet.framenet import Lexeme, LexicalUnit, Frame, FrameNet, FrameElement, FrameRelation

logger = logging.getLogger(__name__)


class FrameXMLParser:
    """
    Parses a XML file describing a Frame into an actual Frame
    """

    _tag2label = {'ex': description.EXample,
                  'fex': description.FEeXample,
                  'fen': description.FEName,
                  't': description.T,
                  'm': description.M,
                  'ment': description.Ment,
                  'gov': description.Gov,
                  'em': description.EM,
                  'supp': description.Supp,
                  'target': description.Target}

    def __init__(self, **args):
        pass

    def _parse_fe(self, xml_node):
        attrib = xml_node.attrib
        for key in attrib:
            attrib[key] = attrib[key].encode('utf-8')
        definition = None
        for grandchild in xml_node:
            if grandchild.tag.endswith('definition'):
                definition = self._parse_description(grandchild)
        fe = FrameElement(name=attrib['name'],
                          abbrev=attrib['abbrev'],
                          definition=definition,
                          fg_color=attrib['fgColor'],
                          bg_color=attrib['bgColor'],
                          is_core=attrib.get('coreType') == 'Core',
                          semantic_type=attrib.get('semanticType', ''),
                          idx=int(attrib.get('ID', 'None')))
        return fe

    def _parse_description(self, xml_node, open_tag='<', close_tag='>', end_tag_marker='/', escape_html=False):
        comment_pattern = compile('{open}!--.*?--{close}'.format(open=open_tag, close=close_tag), DOTALL)
        tag_pattern = compile('(?:{open}(.*?){close})|(.+?(?=(?:{open})|^))'.format(open=open_tag, close=close_tag),
                              DOTALL)
        in_tag_pattern = compile('\s*((?:\w|-|_)+)\s*(?:name="(\w*)")?\s*', DOTALL)

        text = comment_pattern.sub('', xml_node.text)  # if xmlNode.text != None else ''
        tokens = tag_pattern.findall(text)
        tag_stack = []

        desc = description.Description(escape_html=escape_html)
        label_stack = [desc]
        content = None

        logger.debug('SENTENCE:%s', xml_node.text)
        logger.debug('TOKENS:%s', tokens)

        for tag, text in tokens:
            logger.debug("tag:'%s', text: '%s'" % (tag, text))
            if len(tag) != 0:  # if it is a tag
                name, attrib = in_tag_pattern.findall(tag)[0]
                name = name.strip()
                # logger.debug(label_stack, '*', len(label_stack))
                label_buffer = label_stack[-1]
                if content is not None:
                    label_buffer.add_text(content)
                    content = None
                if tag.startswith(end_tag_marker):
                    curr_tag, _ = tag_stack.pop()
                    curr_name, attrib = in_tag_pattern.findall(curr_tag)[0]
                    logger.debug('Last_tag \'%s\', attrib \'%s\', tag \'%s\'', curr_name, attrib, name)
                    if name == curr_name:
                        logger.debug('name is matching!!')
                        if name in self._tag2label:
                            label_stack.pop()
                    else:
                        raise Exception(
                            'Tag is not properly closed: {text}\n{name} != {cname}'.format(name=name, cname=curr_name,
                                                                                           text=xml_node.text))
                else:
                    tag_stack.append((tag, text))
                    try:
                        Label = self._tag2label[name]
                        if len(attrib.strip()) > 0:
                            label_stack.append(Label(escape_html=escape_html, name=attrib))
                        else:
                            label_stack.append(Label(escape_html=escape_html))
                        label_buffer.add_element(label_stack[-1])
                    except KeyError:
                        if name not in ['x', 'b', 'def-root']:
                            logger.warning('Unknown tag "{tag}"'.format(tag=name))
                            # raise Exception('{text}\nWeird tag "{tag}"'.format(tag = name, text= xmlNode.text))
                    if tag.endswith(end_tag_marker):
                        tag_stack.pop()
            else:
                content = text.encode('utf-8')
        if len(tag_stack) > 0:
            logger.warning('Not all tags closed!{stack}'.format(stack=tokens))
        return desc

    @staticmethod
    def _parse_lexeme(xml_node):
        attrib = xml_node.attrib
        for key in attrib:
            attrib[key] = attrib[key].encode('utf-8')
        text = xml_node.text if xml_node.text is not None else ''
        le = Lexeme(name=attrib['name'],
                    pos=attrib['POS'],
                    break_before=attrib['breakBefore'],
                    head_word=attrib['headword'],
                    text=text)
        return le

    def _parse_lu(self, xml_node):
        attrib = xml_node.attrib
        for key in attrib:
            attrib[key] = attrib[key].encode('utf-8')
        definition = ''
        lexeme = None
        for grandchild in xml_node:
            if grandchild.tag.endswith('definition'):
                definition = grandchild.text
            elif grandchild.tag.endswith('sentenceCount'):
                pass
            elif grandchild.tag.endswith('lexeme'):
                lexeme = self._parse_lexeme(grandchild)
        name = '.'.join(attrib['name'].split('.')[:-1])

        lu = LexicalUnit(name=name,
                         pos=attrib['POS'],
                         status=attrib['status'],
                         definition=definition,
                         # annotation = (0,0),
                         idx=int(attrib['ID']),
                         lexeme=lexeme)
        return lu

    def parse(self, file_str):
        tree = XMLTree.parse(file_str).getroot()
        # print tree.attrib, file_str
        name = tree.attrib['name']
        idx = int(tree.attrib['ID'])
        core_fes = []
        peripheral_fes = []
        lus = []
        definition = None
        relations = dict()

        for child in tree:
            if child.tag.endswith('FE'):
                fe = self._parse_fe(child)
                if fe.isCore:
                    core_fes.append(fe)
                else:
                    peripheral_fes.append(fe)

            elif child.tag.endswith('frameRelation'):
                rel_type = child.attrib['type']
                frame_list = []
                for rel_node in child:
                    if rel_node.tag.endswith('relatedFrame'):
                        frame_list.append(rel_node.text)
                relations[rel_type] = FrameRelation(rel_type, frame_list)

            elif child.tag.endswith('lexUnit'):
                lus.append(self._parse_lu(child))

            elif child.tag.endswith('definition'):
                definition = self._parse_description(child)
                # print 'TEXT:', definition#child.text

        return Frame(name=name, description=definition, core_fes=core_fes,
                     peripheral_fes=peripheral_fes, lus=lus, idx=idx, **relations)


class NetXMLParser:
    def __init__(self, frame_parser=FrameXMLParser()):
        self._fparser = frame_parser

    def parse(self, str_dir):
        frames_path = path.join(str_dir, 'frame')
        frames = dict()
        files_names = ls(frames_path)
        for count, file_name in enumerate(ls(frames_path)):
            if file_name.endswith('.xml'):
                file_path = path.join(frames_path, file_name)
                logger.info(
                    '{percent:3d}% reading frame "{absolute}"'.format(percent=((count + 1) * 100) / len(files_names),
                                                                      absolute=file_name))
                frame = self._fparser.parse(file_path)
                frames[frame.name] = frame

        return FrameNet(frames)
