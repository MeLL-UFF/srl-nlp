#!/bin/env python

import argparse
import xml.etree.ElementTree as XMLTree
from sys import argv as _argv

from logger_config import add_logger_args as _add_logger_args, config_logger
from srl_nlp.framenet.corpus import *

logger = logging.getLogger(__name__)


class FNXMLAdapter:
    _TAG_PREFIX = '{http://framenet.icsi.berkeley.edu}'

    def __init__(self, **params):
        self.params = params
        # TODO

    def _parse_document(self, xml_node):
        doc_args = {}
        paragraphs = {}
        logger.debug('ROOT tag:{tag}'.format(tag=xml_node.tag))
        for xml_child in xml_node:
            logger.debug('Child tag:{tag}'.format(tag=xml_child.tag))
            if xml_child.tag == self._TAG_PREFIX + 'header':
                try:
                    xml_corpus_header = xml_child.getchildren()[0]
                    xml_doc_header = xml_corpus_header.getchildren()[0]
                    doc_args['corpus'] = xml_corpus_header.attrib.get('name', '')
                    doc_args['corpusID'] = xml_corpus_header.attrib.get('ID', '')
                    doc_args['id'] = xml_doc_header.attrib.get('ID', '')
                    doc_args['desc'] = xml_doc_header.attrib.get('description', '')
                    doc_args['name'] = xml_doc_header.attrib.get('name', '')
                except IndexError as e:
                    logger.error(e)
            elif self._TAG_PREFIX + 'sentence' == xml_child.tag:
                xml_sent = xml_child
                parNo = int(xml_sent.attrib['paragNo'])
                aPos = int(xml_sent.attrib['aPos'])
                par = paragraphs.get((parNo, aPos), [])
                par.append(self._parse_sentence(xml_sent))
                paragraphs[(parNo, aPos)] = par
        assert len(doc_args) > 0, 'Not enough information to parse a Document'

        ordering = lambda (x, _): x
        ord_par = [par for _, par in sorted(paragraphs.items(), key=ordering)]
        sentences = []
        for par in ord_par:
            sentences.extend(par)
        doc_args['sentences'] = sentences
        doc = Document(**doc_args)
        return doc

    def _parse_sentence(self, xml_node, **params):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'sentence'
        id = xml_node.attrib['ID']
        text = None
        annotation_sets = []
        for child in xml_node:
            if child.tag == self._TAG_PREFIX + 'text':
                text = child.text
            if child.tag == self._TAG_PREFIX + 'annotationSet':
                anno_set = self._parse_annoset(child)
                if anno_set.is_frame():  # TODO use other annotations
                    annotation_sets.append(anno_set)
        sentence = Sentence(id=id, text=text, annotation_sets=annotation_sets)
        return sentence

    def _parse_annoset(self, xml_node, filter=('FE',), **params):
        logger.debug('AnnoSet[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX + 'annotationSet' == xml_node.tag
        layers = []
        for layer in xml_node:
            assert self._TAG_PREFIX + 'layer' == layer.tag
            try:
                layer = self._parse_layer(layer)
                if not filter or (layer.name in filter):
                    layers.append(layer)
            except IndexError:
                continue
        args = {'id': xml_node.attrib['ID'],
                'frameID': xml_node.attrib.get('frameRef', None),
                'frameName': xml_node.attrib.get('frameName', None),
                'luID': xml_node.attrib.get('luID', None),
                'luName': xml_node.attrib.get('luName', None),
                'status': xml_node.attrib.get('status', None),
                'layers': layers}
        params.update(args)
        anno_set = AnnotationSet(**params)
        return anno_set

    def _parse_layer(self, xml_node, **params):
        logger.debug('Layer[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX + 'layer' == xml_node.tag
        annos = []
        for child in xml_node:
            annos.append(self._parse_annotation(child))
            if len(child) == 0:
                err = IndexError('Empty layer id({id})'.format(id=xml_node.attrib.get('ID', '?')))
                logger.warning(err)
        args = {'rank': xml_node.attrib.get('rank', None),
                'name': xml_node.attrib['name'],
                'annotations': annos}
        params.update(args)
        return Layer(**params)

    def _parse_annotation(self, xml_node, **params):
        # TODO change for the layer level
        logger.debug('Annotation[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX + 'label' == xml_node.tag
        label = xml_node
        logger.debug('{lab}:{attr}'.format(lab=label.tag, attr=label.attrib))
        name = label.attrib['name']
        itype = label.attrib.get('itype', None)
        if itype:
            anno = Annotation(name=name, itype=itype, **params)
        else:
            start = label.attrib['start']
            end = label.attrib['end']
            anno = Annotation(start=start, end=end, name=name, **params)
        return anno

    def parseXML(self, xml_item):
        """
        Returns a list of documents
        """
        if isinstance(xml_item, XMLTree.Element):
            root = xml_item
        else:
            root = XMLTree.parse(xml_item).getroot()
        return [self._parse_document(root)]

    def doc2XML(self, doc, xml_file=None):
        root = self._doc2XML(doc)
        tree = XMLTree.ElementTree(root)
        if xml_file is not None:
            tree.write(xml_file, encoding='utf-8', xml_declaration=True)
        else:
            return XMLTree.tostring(tree, encoding='utf-8')

    def _doc2XML(self, doc):
        xml_root = XMLTree.Element('fullTextAnnotation')
        xml_root.attrib = {'xmlns': 'http://framenet.icsi.berkeley.edu',
                           'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance'}
        xml_header = XMLTree.Element('header')
        xml_header.attrib = {'xmlns': 'http://framenet.icsi.berkeley.edu',
                             'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance'}
        xml_corpus = XMLTree.Element('corpus')
        xml_corpus.attrib = {'description': '',
                             'name': doc.corpus, 'ID': doc.corpusID}
        xml_doc = XMLTree.Element('document')
        xml_doc.attrib = {'description': doc.desc,
                          'name': doc.name, 'ID': doc.id}
        xml_corpus.append(xml_doc)
        xml_header.append(xml_corpus)
        xml_root.append(xml_header)

        for sent in doc.sentences:
            xml_root.append(self._sentence2XML(sent, corpID=doc.corpusID, docID=doc.id))

        return xml_root

    def _sentence2XML(self, sent, corpID, docID):
        xml_sent = XMLTree.Element('sentence')
        xml_sent.attrib = {'corpID': corpID,
                           'docID': docID,
                           # 'sentNo': None,
                           # 'paragNo': None,
                           # 'aPos': None,
                           'ID': sent.id}
        xml_text = XMLTree.Element('text')
        xml_text.text = sent.text
        xml_sent.append(xml_text)
        for annoSet in sent.annotation_sets:  # TODO
            xml_sent.append(self._anno_set2XML(annoSet))
        return xml_sent

    def _anno_set2XML(self, annoset):
        xml_anno_set = XMLTree.Element('annotationSet')
        xml_anno_set.attrib = {'ID': annoset.id}
        for layer in annoset:
            xml_anno_set.append(self._layer2XML(layer))
        return xml_anno_set

    def _layer2XML(self, layer):
        xml_layer = XMLTree.Element('layer')
        xml_layer.attrib = {'name': layer.name}
        if layer.rank is not None:
            xml_layer.attrib['rank'] = layer.rank
        for label in layer:
            xml_layer.append(self._anno2XML(label))
        return xml_layer

    def _anno2XML(self, anno):
        xml_anno = XMLTree.Element('label')
        xml_anno.attrib = {'start': anno.start,
                           'end': anno.end,
                           'itype': anno.itype,
                           'name': anno.name}
        for key, val in xml_anno.attrib.items():
            if val is None:
                del (xml_anno.attrib[key])
        return xml_anno


######------------------------------

class SemEval07XMLAdapter(FNXMLAdapter):
    _TAG_PREFIX = ''

    def __init__(self, **params):
        FNXMLAdapter.__init__(self, **params)
        self.params = params

    def _parse_document(self, xml_node):
        id = xml_node.attrib['ID']
        desc = xml_node.attrib['description']
        paragraphs = {}
        for xml_par_list in xml_node:
            assert 'paragraphs' == xml_par_list.tag
            for xml_par in xml_par_list:
                assert 'paragraph' == xml_par.tag
                sentences = []
                for xml_sents in xml_par:
                    assert 'sentences' == xml_sents.tag
                    for xml_sent in xml_sents:
                        sentences.append(self._parse_sentence(xml_sent))
                pos = int(xml_par.attrib['documentOrder'])
                sentences.extend(paragraphs.get(pos, []))
                paragraphs[pos] = sentences
        ordering = lambda (x, _): x
        sentences = []
        for _, sent_list in sorted(paragraphs.items(), key=ordering):
            sentences.extend(sent_list)
        doc = Document(id=id, desc=desc, sentences=sentences)
        return doc

    def _parse_annoset(self, xml_node, **params):
        logger.debug(xml_node.tag)
        assert 'annotationSet' == xml_node.tag
        annos = []
        for layers in xml_node:
            for layer in layers:
                # TODO assert layer has type layer
                try:
                    annos.append(self._parse_annotation(layer))
                except IndexError:
                    continue

        id = xml_node.get('ID', None)
        frameID = xml_node.get('frameRef', None)
        frameName = xml_node.get('frameName', None)
        luID = xml_node.get('lexUnitRef', None)
        luName = xml_node.get('luName', None)
        status = xml_node.get('status', None)

        anno_set = AnnotationSet(id=id,
                                 frameID=frameID,
                                 frameName=frameName,
                                 luID=luID,
                                 luName=luName,
                                 status=status,
                                 annotations=annos,
                                 **params)
        return anno_set

    def parseXML(self, xml_item):
        """
        Returns a list of documents
        """
        if isinstance(xml_item, XMLTree.Element):
            root = xml_item
        else:
            root = XMLTree.parse(xml_item).getroot()
        logger.debug('{}: {}'.format(root.tag, root.attrib))
        c_name = root.attrib['name']
        c_id = root.attrib['ID']
        docs = []
        for doc_list in root:
            for xml_doc in doc_list:
                doc = self._parse_document(xml_doc)
                doc.corpus = c_name
                doc.corpusID = c_id
                docs.append(doc)
        return docs


PARSERS_AVAILABLE = {'semeval': SemEval07XMLAdapter,
                     'framenet': FNXMLAdapter}


def parse_args(argv=_argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='Parses the semeval07 and framenet format into Documment')
    parser.add_argument('input_file', help='File to be parsed')
    parser.add_argument('-o', '--output_file', help='File to write the pickle serialization')
    parser.add_argument('-c', '--check_examples', action='store_true',
                        help='check if all examples are a perfect parsing of the respective sentence')
    # parser.add_argument('-t', '--examples_as_text', help = 'Convert the examples to text before printing')
    parser.add_argument('-x', '--output_xml_file', help='XML File to write the information extracted')
    parser.add_argument("-p", "--parser", choices=PARSERS_AVAILABLE.keys(),
                        help='Parser for the appropriate kind of file')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


def main(argv):
    args = parse_args(argv, _add_logger_args)
    config_logger(args)
    logger.info('Loading XML file')
    with open(args.input_file, 'r') as f:
        tree = XMLTree.parse(f)
    logger.info('XML tree ready')
    root = tree.getroot()

    adapter = PARSERS_AVAILABLE.get(args.parser, SemEval07XMLAdapter)()

    logger.info('Parsing XML tree')
    try:
        docs = adapter.parseXML(root)
    except KeyError as e:
        raise KeyError('Consider using another parser type by using the option --parser')
    logger.info('Done parsing XML tree')

    if args.check_examples:
        for doc in docs:
            for sentence in doc:
                converted = sentence.get_fn_example().str_no_annotation()
                print converted
                print sentence.get_fn_example()
                # raw_input()
                if converted != sentence.text:
                    logger.critical("{sent} was not properly processed".format(sent=sentence))

    if args.output_file is not None:
        logger.info('Writing pickle file')
        with open(args.output_file, 'wb') as f:
            pickle.dump(docs, f)

    if args.output_xml_file is not None:
        logger.info('Writing XML file')
        with open(args.output_xml_file, 'w') as f:
            doc = docs[0]  # TODO iterate over the list
            adapter.doc2XML(doc, xml_file=f)


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
