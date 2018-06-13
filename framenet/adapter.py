#!/bin/env python2

import argparse
import xml.etree.ElementTree as XMLTree
from sys import argv as _argv

from logger_config import add_logger_args as _add_logger_args, config_logger
from rule_utils import not_none_to_str
from srl_nlp.framenet.corpus import *

logger = logging.getLogger(__name__)


class FNXMLAdapter(object):
    _TAG_PREFIX = '{http://framenet.icsi.berkeley.edu}'

    def __init__(self, encoding=None, **params):
        self.params = params
        self.encoding = encoding

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
                    doc_args['corpus_id'] = xml_corpus_header.attrib.get('ID', '')
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
        ord_par = [Paragraph(i, sentences=par) for (i, j), par in sorted(paragraphs.items(), key=ordering)]
        doc_args['elements'] = ord_par
        doc = Document(**doc_args)
        return doc

    def _parse_sentence(self, xml_node, encoding=None):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'sentence'
        id = xml_node.attrib['ID']
        text = None
        annotation_sets = []
        words = []
        if not encoding:
            encoding = self.encoding
        for field in xml_node:
            if field.tag == self._TAG_PREFIX + 'text':
                text = field.text.encode(encoding)
                # TODO parse words
            elif field.tag == self._TAG_PREFIX + 'annotationSet':
                anno_set = self._parse_annoset(field)
                if anno_set.is_frame():
                    annotation_sets.append(anno_set)
                else:
                    for layer in anno_set:
                        if layer.name == "PENN":
                            for label in layer:
                                words.append((label.start, label.end))
        sentence = Sentence(id=id, text=text, annotation_sets=annotation_sets, parts_of_speech=words)
        return sentence

    def _parse_annoset(self, xml_node, **params):
        logger.debug('AnnoSet[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX + 'annotationSet' == xml_node.tag
        layers = []
        for layer in xml_node:
            assert self._TAG_PREFIX + 'layer' == layer.tag
            try:
                layer = self._parse_layer(layer)
                layers.append(layer)
            except IndexError:
                continue

        id = xml_node.get('ID', None)
        frameID = xml_node.get('frameID', None)
        frameName = xml_node.get('frameName', None)
        luID = xml_node.get('luID', None)
        luName = xml_node.get('luName', None)
        status = xml_node.get('status', None)

        anno_set = AnnotationSet(id=id,
                                 frame_id=frameID,
                                 frame_name=frameName,
                                 lu_id=luID,
                                 lu_name=luName,
                                 status=status,
                                 layers=layers,
                                 **params)
        return anno_set

    def _parse_layer(self, xml_node, **params):
        logger.debug('Layer[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX + 'layer' == xml_node.tag
        annos = []
        for child in xml_node:
            if child.tag == self._TAG_PREFIX + 'labels':
                for label in child:
                    annos.append(self._parse_annotation(label))
            elif child.tag == self._TAG_PREFIX + 'label':
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

    def doc2XML(self, doc, xml_file=None, encoding=None):
        root = self._doc2XML(doc)
        tree = XMLTree.ElementTree(root)
        if not encoding:
            encoding = self.encoding
        if xml_file is not None:
            tree.write(xml_file, encoding=encoding, xml_declaration=True)
        else:
            return XMLTree.tostring(tree, encoding=encoding)

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

        for sent in doc.elements:
            xml_root.append(self._sentence2XML(sent, corpID=doc.corpusID, docID=doc.id))

        return xml_root

    def _sentence2XML(self, sent, **kwargs):
        xml_sent = XMLTree.Element('sentence')
        xml_sent.attrib = {'ID': not_none_to_str(sent.id)}
        xml_sent.attrib.update(kwargs)
        xml_text = XMLTree.Element('text')
        xml_text.text = sent.text.decode(self.encoding)
        xml_sent.append(xml_text)
        for annoSet in sent.annotation_sets:
            xml_sent.append(self._anno_set2XML(annoSet))
        return xml_sent

    def _anno_set2XML(self, annoset):
        xml_anno_set = XMLTree.Element('annotationSet')
        xml_anno_set.attrib = {'ID': not_none_to_str(annoset.id),
                               'status': not_none_to_str(annoset.status),
                               'frameName': not_none_to_str(annoset.frameName),
                               'luName': not_none_to_str(annoset.luName),
                               }
        xml_anno_set.attrib = {k: value for (k, value) in xml_anno_set.attrib.items() if value is not None}
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
        xml_anno.attrib = {'start': not_none_to_str(anno.start),
                           'end': not_none_to_str(anno.end),
                           'itype': not_none_to_str(anno.itype),
                           'name': not_none_to_str(anno.name)}
        for key, val in xml_anno.attrib.items():
            if val is None:
                del (xml_anno.attrib[key])
        return xml_anno


######------------------------------

class SemEval07XMLAdapter(FNXMLAdapter):
    _TAG_PREFIX = ''

    def __init__(self, encoding='utf-8', **params):
        FNXMLAdapter.__init__(self, **params)
        self.params = params
        self.encoding = encoding

    def _parse_document(self, xml_node):
        logger.debug('Document({}):{}'.format(xml_node.tag, xml_node.attrib))
        id = xml_node.attrib['ID']
        desc = xml_node.attrib['description']
        paragraphs = []
        for xml_par_list in xml_node:
            assert 'paragraphs' == xml_par_list.tag
            for xml_par in xml_par_list:
                paragraphs.append(self._parse_paragraph(xml_par))
        doc = Document(id=id, desc=desc, elements=paragraphs)
        return doc

    def _parse_paragraph(self, xml_node):
        logger.debug('Paragraph({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'paragraph'
        id = xml_node.attrib['ID']
        sentences = []
        for sent_list in xml_node:
            assert sent_list.tag == self._TAG_PREFIX + 'sentences'
            for sent in sent_list:
                sentences.append(self._parse_sentence(sent))
        paragraph = Paragraph(id, sentences)
        return paragraph

    def _parse_sentence(self, xml_node, encoding=None):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'sentence'
        id = xml_node.attrib['ID']
        text = None
        annotation_sets = []
        words = []
        if not encoding:
            encoding = self.encoding
        for field in xml_node:
            if field.tag == self._TAG_PREFIX + 'text':
                text = field.text.encode(encoding)
            elif field.tag == self._TAG_PREFIX + 'parts-of-speech':
                for pos in field:
                    token_pos = pos.attrib['start'], pos.attrib['end']
                    words.append(token_pos)
            elif field.tag == self._TAG_PREFIX + 'annotationSets':
                for anno_set_node in field:
                    anno_set = self._parse_annoset(anno_set_node)
                    if anno_set.is_frame():
                        annotation_sets.append(anno_set)
        assert text is not None
        sentence = Sentence(id=id, text=text, annotation_sets=annotation_sets, parts_of_speech=words)
        return sentence

    def _parse_annoset(self, xml_node, **params):
        logger.debug(xml_node.tag)
        assert 'annotationSet' == xml_node.tag
        layers = []
        for xml_layers in xml_node:
            for xml_layer in xml_layers:
                try:
                    layers.append(self._parse_layer(xml_layer))
                except IndexError:
                    continue

        id = xml_node.get('ID', None)
        frameID = xml_node.get('frameRef', None)
        frameName = xml_node.get('frameName', None)
        luID = xml_node.get('lexUnitRef', None)
        luName = xml_node.get('luName', None)
        status = xml_node.get('status', None)

        anno_set = AnnotationSet(id=id,
                                 frame_id=frameID,
                                 frame_name=frameName,
                                 lu_id=luID,
                                 lu_name=luName,
                                 status=status,
                                 layers=layers,
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

    def _doc2XML(self, doc):
        xml_corpus = XMLTree.Element('corpus')
        xml_corpus.attrib = {'description': '',
                             'name': doc.corpus, 'ID': doc.corpusID}
        xml_docs = XMLTree.Element('documents')
        xml_doc = XMLTree.Element('document')
        xml_doc.attrib = {'description': not_none_to_str(doc.desc),
                          'name': not_none_to_str(doc.name),
                          'ID': not_none_to_str(doc.id)}
        xml_pars = XMLTree.Element('paragraphs')
        for i, paragraph in enumerate(doc.elements):
            xml_pars.append(self._paragraph2XML(paragraph, document_order=str(i + 1)))

        xml_doc.append(xml_pars)
        xml_docs.append(xml_doc)
        xml_corpus.append(xml_docs)
        return xml_corpus

    def _paragraph2XML(self, paragraph, document_order=""):
        xml_par = XMLTree.Element('paragraph')
        xml_par.attrib = {'documentOrder': not_none_to_str(document_order),
                          'ID': not_none_to_str(paragraph.id)}
        xml_sents = XMLTree.Element('sentences')
        for sent in paragraph.sentences:
            xml_sents.append(self._sentence2XML(sent))
        xml_par.append(xml_sents)
        return xml_par

    def _sentence2XML(self, sent, **kwargs):
        xml_sent = XMLTree.Element('sentence')
        xml_sent.attrib = {'ID': sent.id}
        xml_sent.attrib.update(kwargs)

        xml_text = XMLTree.Element('text')
        xml_text.text = sent.text.decode(self.encoding)

        xml_pos = XMLTree.Element('parts-of-speech')
        for word in sent.parts_of_speech:
            attrib = {'start': not_none_to_str(word[0]),
                      'end': not_none_to_str(word[1])}
            xml_pos.append(XMLTree.Element('pos', attrib=attrib))

        xml_anno_sets = XMLTree.Element('annotationSets')
        for annoSet in sent.annotation_sets:
            xml_anno_sets.append(self._anno_set2XML(annoSet))

        xml_sent.append(xml_text)
        xml_sent.append(xml_pos)
        xml_sent.append(xml_anno_sets)

        return xml_sent

    def _layer2XML(self, layer):
        xml_layer = XMLTree.Element('layer')
        xml_layer.attrib = {'name': layer.name}
        xml_labels = XMLTree.Element('labels')
        if layer.rank is not None:
            xml_layer.attrib['rank'] = layer.rank
        for label in layer:
            xml_labels.append(self._anno2XML(label))
        xml_layer.append(xml_labels)
        return xml_layer

    def _anno_set2XML(self, annoset):
        xml_anno_set = XMLTree.Element('annotationSet')
        xml_anno_set.attrib = {'ID': not_none_to_str(annoset.id),
                               'status': not_none_to_str(annoset.status),
                               'frameName': not_none_to_str(annoset.frameName),
                               'luName': not_none_to_str(annoset.luName),
                               }
        xml_anno_set.attrib = {k: value for (k, value) in xml_anno_set.attrib.items() if value is not None}
        xml_layers = XMLTree.Element('layers')
        for layer in annoset:
            xml_layers.append(self._layer2XML(layer))
        xml_anno_set.append(xml_layers)
        return xml_anno_set


PARSERS_AVAILABLE = {'semeval': SemEval07XMLAdapter,
                     'framenet': FNXMLAdapter}


def parse_args(argv=_argv, add_logger_args=lambda x: None):
    parser = argparse.ArgumentParser(description='Parses the semeval07 and framenet format into the Corpus format')
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
    except KeyError:
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
            for doc in docs:
                adapter.doc2XML(doc, xml_file=f)


if __name__ == '__main__':
    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
