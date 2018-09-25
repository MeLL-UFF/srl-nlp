#!/bin/env python2
from itertools import chain

import json
import logging
import xml.etree.ElementTree as XMLTree
from typing import List
from srl_nlp.framenet.corpus import Document, Sentence, AnnotationSet, Annotation, Paragraph, Layer
from srl_nlp.rule_utils import not_none_to_str
from abc import abstractmethod

logger = logging.getLogger(__name__)


class DocumentAdapter(object):
    def __init__(self, encoding='utf-8', **params):
        self.params = params
        self.encoding = encoding

    @abstractmethod
    def parse_file(self, file_or_file_name):
        # type: (str) -> List[Document]
        pass

    @abstractmethod
    def write_doc(self, doc, file_or_file_name, encoding=None):
        # type: (Document, any, str) -> None
        pass

    @abstractmethod
    def doc_to_string(self, doc, encoding=None):
        # type: (Document, str) -> str
        pass

    @property
    @abstractmethod
    def file_format(self):
        pass


# --------------------------------------------


class FNXMLAdapter(DocumentAdapter):
    """
    FrameNet XML document XML file format Adapter (FNXMLAdapter) is responsible for reading the documents written in
    this format and create a Document object.
    """

    _TAG_PREFIX = '{http://framenet.icsi.berkeley.edu}'

    @property
    def file_format(self):
        return 'xml'

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
                    doc_args['doc_id'] = xml_doc_header.attrib.get('ID', '')
                    doc_args['desc'] = xml_doc_header.attrib.get('description', '')
                    doc_args['name'] = xml_doc_header.attrib.get('name', '')
                except IndexError as ex:
                    logger.error(ex)
            elif self._TAG_PREFIX + 'sentence' == xml_child.tag:
                xml_sent = xml_child
                par_no = int(xml_sent.attrib['paragNo'])
                a_pos = int(xml_sent.attrib['aPos'])
                par = paragraphs.get((par_no, a_pos), [])
                par.append(self._parse_sentence(xml_sent))
                paragraphs[(par_no, a_pos)] = par
        assert len(doc_args) > 0, 'Not enough information to parse a Document'

        def get_first_of_pair(pair):
            return pair[0]

        ord_par = [Paragraph(i, sentences=par) for (i, j), par in sorted(paragraphs.items(), key=get_first_of_pair)]
        doc_args['elements'] = ord_par
        doc = Document(**doc_args)
        return doc

    def _parse_sentence(self, xml_node, encoding=None):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'sentence'
        idx = xml_node.attrib['ID']
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
        sentence = Sentence(sent_id=idx, text=text, annotation_sets=annotation_sets, parts_of_speech=words)
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

        idx = xml_node.get('ID', None)
        frame_id = xml_node.get('frameID', None)
        frame_name = xml_node.get('frameName', None)
        lu_id = xml_node.get('luID', None)
        lu_name = xml_node.get('luName', None)
        status = xml_node.get('status', None)

        anno_set = AnnotationSet(anno_set_id=idx,
                                 frame_id=frame_id,
                                 frame_name=frame_name,
                                 lu_id=lu_id,
                                 lu_name=lu_name,
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
        return Layer(rank=xml_node.attrib.get('rank', None),
                     name=xml_node.attrib['name'],
                     annotations=annos,
                     **params)

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

    def parse_file(self, file_or_file_name):
        """
        Returns a list of documents
        """
        if isinstance(file_or_file_name, XMLTree.Element):
            root = file_or_file_name
        else:
            root = XMLTree.parse(file_or_file_name).getroot()
        return [self._parse_document(root)]

    def write_doc(self, doc, file_or_file_name, encoding=None):
        # type: (Document, any, str) -> None
        root = self._doc2XML(doc)
        tree = XMLTree.ElementTree(root)
        if not encoding:
            encoding = self.encoding
        tree.write(file_or_file_name, encoding=encoding, xml_declaration=True)

    def doc_to_string(self, doc, encoding=None):
        # type: (Document, str) -> object
        root = self._doc2XML(doc)
        if not encoding:
            encoding = self.encoding
        return XMLTree.tostring(root, encoding=encoding)

    def _doc2XML(self, doc):
        xml_root = XMLTree.Element('fullTextAnnotation')
        xml_root.attrib = {'xmlns': 'http://framenet.icsi.berkeley.edu',
                           'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance'}
        xml_header = XMLTree.Element('header')
        xml_header.attrib = {'xmlns': 'http://framenet.icsi.berkeley.edu',
                             'xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance'}
        xml_corpus = XMLTree.Element('corpus')
        xml_corpus.attrib = {'description': '',
                             'name': doc.corpus, 'ID': str(doc.corpusID)}
        xml_doc = XMLTree.Element('document')
        xml_doc.attrib = {'description': doc.desc,
                          'name': doc.name, 'ID': str(doc.id)}
        xml_corpus.append(xml_doc)
        xml_header.append(xml_corpus)
        xml_root.append(xml_header)

        for elem in doc.elements:
            if isinstance(elem, Sentence):
                sent = elem  # type: Sentence
                xml_root.append(self._sentence2XML(sent, corpID=doc.corpusID, docID=doc.id))
            elif isinstance(elem, Paragraph):
                par = elem  # type: Paragraph
                for sent in par.sentences:
                    xml_root.append(self._sentence2XML(sent, corpID=doc.corpusID, docID=doc.id))
            else:
                raise Exception('Invalid object passed as document.element: {}: {}.\n'
                                'It should be a Sentence or Paragraph object'.format(elem, type(elem)))

        return xml_root

    def _sentence2XML(self, sent, **kwargs):
        xml_sent = XMLTree.Element('sentence')
        xml_sent.attrib = {'ID': not_none_to_str(sent.id)}
        kwargs = {k: str(v) for k, v in kwargs.items()}
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
        xml_anno_set.attrib = {k: str(v) for k, v in xml_anno_set.attrib.items() if v is not None}
        for layer in annoset:
            xml_anno_set.append(self._layer2XML(layer))
        return xml_anno_set

    def _layer2XML(self, layer):
        xml_layer = XMLTree.Element('layer')
        xml_layer.attrib = {'name': layer.name}
        if layer.rank is not None:
            xml_layer.attrib['rank'] = str(layer.rank)
        for label in layer:
            xml_layer.append(self._anno2XML(label))
        return xml_layer

    def _anno2XML(self, anno):
        xml_anno = XMLTree.Element('label')
        attrib = {'start': not_none_to_str(anno.start),
                  'end': not_none_to_str(anno.end),
                  'itype': not_none_to_str(anno.itype),
                  'name': not_none_to_str(anno.name)}
        attrib = {k: v for k, v in attrib.items() if v is not None}
        xml_anno.attrib = attrib
        return xml_anno


# --------------------------------------------


class SemEval07XMLAdapter(FNXMLAdapter):
    """
    SemEval 2017 Task 19 document XML file format Adapter (SemEval07XMLAdapter) is responsible for reading the documents
    written in this format and create a Document object.
    """

    _TAG_PREFIX = ''

    def _parse_document(self, xml_node):
        logger.debug('Document({}):{}'.format(xml_node.tag, xml_node.attrib))
        idx = xml_node.attrib['ID']
        desc = xml_node.attrib['description']
        paragraphs = []
        for xml_par_list in xml_node:
            assert 'paragraphs' == xml_par_list.tag
            for xml_par in xml_par_list:
                paragraphs.append(self._parse_paragraph(xml_par))
        doc = Document(doc_id=idx, desc=desc, elements=paragraphs)
        return doc

    def _parse_paragraph(self, xml_node):
        logger.debug('Paragraph({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'paragraph'
        idx = xml_node.attrib['ID']
        sentences = []
        for sent_list in xml_node:
            assert sent_list.tag == self._TAG_PREFIX + 'sentences'
            for sent in sent_list:
                sentences.append(self._parse_sentence(sent))
        paragraph = Paragraph(idx, sentences)
        return paragraph

    def _parse_sentence(self, xml_node, encoding=None):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX + 'sentence'
        idx = xml_node.attrib['ID']
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
        sentence = Sentence(sent_id=idx, text=text, annotation_sets=annotation_sets, parts_of_speech=words)
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

        idx = xml_node.get('ID', None)
        frame_id = xml_node.get('frameRef', None)
        frame_name = xml_node.get('frameName', None)
        lu_id = xml_node.get('lexUnitRef', None)
        lu_name = xml_node.get('luName', None)
        status = xml_node.get('status', None)

        anno_set = AnnotationSet(anno_set_id=idx,
                                 frame_id=frame_id,
                                 frame_name=frame_name,
                                 lu_id=lu_id,
                                 lu_name=lu_name,
                                 status=status,
                                 layers=layers,
                                 **params)
        return anno_set

    def parse_file(self, file_or_file_name):
        """
        Returns a list of documents
        """
        if isinstance(file_or_file_name, XMLTree.Element):
            root = file_or_file_name
        else:
            root = XMLTree.parse(file_or_file_name).getroot()
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


# --------------------------------------------


class JSONAdapter(DocumentAdapter):
    """
    SemEval 2017 Task 19 document XML file format Adapter (SemEval07XMLAdapter) is responsible for reading the documents
    written in this format and create a Document object.
    """

    _TAG_PREFIX = ''

    @property
    def file_format(self):
        return 'json'

    def __init__(self, encoding='utf-8', method='SEMAFOR', **params):
        DocumentAdapter.__init__(self, encoding=encoding, **params)
        self.method = method

    def _parse_document(self, json_obj):
        idx = json_obj.get('id', '0')
        desc = json_obj.get('description', '')
        logger.debug('Document({}):{}'.format(idx, desc))
        paragraphs = list(map(self._parse_paragraph, json_obj.get('paragraphs')))

        doc = Document(doc_id=idx, desc=desc, elements=paragraphs)
        return doc

    def _parse_paragraph(self, json_obj):
        idx = json_obj.get('id', '0')
        logger.debug('Paragraph({})'.format(idx))
        sentences = list(map(self.parse_sentence, json_obj.get('sentences')))
        paragraph = Paragraph(idx, sentences)
        return paragraph

    def parse_sentence(self, json_obj):
        idx = json_obj.get('id', '0')
        json_frames = json_obj['frames']
        tokens = json_obj['tokens']
        anno_sets = list(chain(*map(self.parse_annotations, json_frames)))
        words = []
        char_count = 0
        for i in range(len(tokens)):  # TODO check this
            token = tokens[0]
            start = char_count + 1
            char_count = char_count + 1 + len(token)
            words.append((start, char_count))
        # logger.debug('Sentence({}):{}'.format(idx, desc))

        sentence = Sentence(sent_id=idx, text=' '.join(tokens), annotation_sets=anno_sets, parts_of_speech=words)
        return sentence

    def parse_annotations(self, json_frame):  # TODO
        json_anno_sets = json_frame['annotationSets']
        json_target = json_frame['target']
        frame_name = json_target['name']

        anno_sets = []
        for json_anno_set in json_anno_sets:
            rank = json_anno_set['rank']

            logger.debug('Frame({})'.format(json_target))
            layers = []

            target_start = json_target['spans'][0]['start']  # TODO only 0 ?
            target_end = json_target['spans'][0]['end']
            layers.append(Layer(rank=str(0), name='Target',
                                annotations=[Annotation(name='Target',
                                                        start=target_start,
                                                        end=target_end)]))
            annotations = []
            for json_annotation in json_anno_set['frameElements']:
                name = json_annotation['name']
                for span in json_annotation['spans']:
                    start = span['start']  # TODO this start has to be calculated for the new pattern
                    end = span['end']  # TODO  this end has to be calculated for the new pattern
                    annotation = Annotation(name=name, start=start, end=end)
                    annotations.append(annotation)
            layers.append(Layer(name='FE', rank=str(rank), annotations=annotations))
            anno_set = AnnotationSet(anno_set_id='0',  # TODO use at least a count
                                     frame_name=frame_name, layers=layers,
                                     status=self.method)  # TODO which anno id?
            anno_sets.append(anno_set)

        return anno_sets

    def parse_file(self, file_or_file_name):
        """
        Returns a list of documents
        """
        if hasattr(file_or_file_name, 'read'):
            json_obj = json.load(file_or_file_name)
        elif hasattr(file_or_file_name, 'read'):
            json_obj = json.load(file_or_file_name)
        else:
            with open(file_or_file_name, 'r') as f:
                json_obj = json.load(f)

        return list(map(self._parse_document, json_obj['documents']))

    def write_doc(self, doc, file_or_file_name, encoding=None):
        # type: (Document, any, str) -> None
        json_obj = None  # self._doc_to_json(doc) TODO
        if hasattr(file_or_file_name, 'read'):
            json.dump(json_obj, file_or_file_name)
        else:
            with open(file_or_file_name, 'r') as f:
                json.dump(json_obj, f)

    def doc_to_string(self, doc, encoding=None):
        # type: (Document, str) -> str
        json_obj = None  # self._doc_to_json(doc) # TODO
        return json.dumps(json_obj)

    # def _doc_to_json(self, doc):  # TODO
    #     xml_corpus = XMLTree.Element('corpus')
    #     xml_corpus.attrib = {'description': '',
    #                          'name': doc.corpus, 'ID': doc.corpusID}
    #     xml_docs = XMLTree.Element('documents')
    #     xml_doc = XMLTree.Element('document')
    #     xml_doc.attrib = {'description': not_none_to_str(doc.desc),
    #                       'name': not_none_to_str(doc.name),
    #                       'ID': not_none_to_str(doc.id)}
    #     xml_pars = XMLTree.Element('paragraphs')
    #     for i, paragraph in enumerate(doc.elements):
    #         xml_pars.append(self._paragraph2XML(paragraph, document_order=str(i + 1)))
    #
    #     xml_doc.append(xml_pars)
    #     xml_docs.append(xml_doc)
    #     xml_corpus.append(xml_docs)
    #     return xml_corpus
    #
    # def _paragraph2XML(self, paragraph, document_order=""):  # TODO
    #     xml_par = XMLTree.Element('paragraph')
    #     xml_par.attrib = {'documentOrder': not_none_to_str(document_order),
    #                       'ID': not_none_to_str(paragraph.id)}
    #     xml_sents = XMLTree.Element('sentences')
    #     for sent in paragraph.sentences:
    #         xml_sents.append(self._sentence2XML(sent))
    #     xml_par.append(xml_sents)
    #     return xml_par
    #
    # def _sentence2XML(self, sent, **kwargs):  # TODO
    #     xml_sent = XMLTree.Element('sentence')
    #     xml_sent.attrib = {'ID': sent.id}
    #     xml_sent.attrib.update(kwargs)
    #
    #     xml_text = XMLTree.Element('text')
    #     xml_text.text = sent.text.decode(self.encoding)
    #
    #     xml_pos = XMLTree.Element('parts-of-speech')
    #     for word in sent.parts_of_speech:
    #         attrib = {'start': not_none_to_str(word[0]),
    #                   'end': not_none_to_str(word[1])}
    #         xml_pos.append(XMLTree.Element('pos', attrib=attrib))
    #
    #     xml_anno_sets = XMLTree.Element('annotationSets')
    #     for annoSet in sent.annotation_sets:
    #         xml_anno_sets.append(self._anno_set2XML(annoSet))
    #
    #     xml_sent.append(xml_text)
    #     xml_sent.append(xml_pos)
    #     xml_sent.append(xml_anno_sets)
    #
    #     return xml_sent

    # def _layer2XML(self, layer):  # TODO
    #     xml_layer = XMLTree.Element('layer')
    #     xml_layer.attrib = {'name': layer.name}
    #     xml_labels = XMLTree.Element('labels')
    #     if layer.rank is not None:
    #         xml_layer.attrib['rank'] = layer.rank
    #     for label in layer:
    #         xml_labels.append(self._anno2XML(label))
    #     xml_layer.append(xml_labels)
    #     return xml_layer
    #
    # def _anno_set2XML(self, annoset):  # TODO
    #     xml_anno_set = XMLTree.Element('annotationSet')
    #     xml_anno_set.attrib = {'ID': not_none_to_str(annoset.id),
    #                            'status': not_none_to_str(annoset.status),
    #                            'frameName': not_none_to_str(annoset.frameName),
    #                            'luName': not_none_to_str(annoset.luName),
    #                            }
    #     xml_anno_set.attrib = {k: value for (k, value) in xml_anno_set.attrib.items() if value is not None}
    #     xml_layers = XMLTree.Element('layers')
    #     for layer in annoset:
    #         xml_layers.append(self._layer2XML(layer))
    #     xml_anno_set.append(xml_layers)
    #     return xml_anno_set


PARSERS_AVAILABLE = {'semeval': SemEval07XMLAdapter,
                     'framenet': FNXMLAdapter,
                     'json': JSONAdapter}

if __name__ == '__main__':  # TODO Move this to proper script
    import argparse
    import pickle
    from sys import argv as _argv
    from srl_nlp.logger_config import add_logger_args as _add_logger_args, config_logger


    def parse_args(argv=_argv, add_logger_args=lambda x: None):
        parser = argparse.ArgumentParser(description='Parses the semeval07 and framenet format into the Corpus format')
        parser.add_argument('input_file', help='File to be parsed')
        parser.add_argument('-p', '--pickle_output_file', help='File to write the pickle serialization')
        parser.add_argument('-c', '--check_examples', action='store_true',
                            help='check if all examples are a perfect parsing of the respective sentence')
        # parser.add_argument('-t', '--examples_as_text', help = 'Convert the examples to text before printing')
        parser.add_argument('-o', '--output_file', help='File to write the information extracted')
        parser.add_argument("--parser_in", choices=PARSERS_AVAILABLE.keys(),
                            help='Parser for the appropriate kind of file'),
        parser.add_argument("--parser_out", default='none', choices=list(PARSERS_AVAILABLE.keys()) + ['none'],
                            help='Parser for writing the appropriate kind of file')
        add_logger_args(parser)
        args = parser.parse_args(argv[1:])
        return args


    def main(argv):
        args = parse_args(argv, _add_logger_args)
        config_logger(args)
        adapter_in = PARSERS_AVAILABLE.get(args.parser_in, SemEval07XMLAdapter)()
        if args.parser_out in PARSERS_AVAILABLE:
            adapter_out = PARSERS_AVAILABLE[args.parser_out]()
        else:
            adapter_out = adapter_in

        logger.info('Parsing {} file'.format(adapter_in.file_format).upper())
        try:
            docs = adapter_in.parse_file(args.input_file)
        except (ValueError, KeyError) as exception:
            raise Exception(
                'Consider using another parser type by using the option --parser_in\n{}'.format(str(exception)))
        logger.info('Done parsing {} file'.format(adapter_in.file_format).upper())

        if args.check_examples:
            for doc in docs:
                for sentence in doc:
                    converted = sentence.get_fn_example().str_no_annotation()
                    print converted
                    print sentence.get_fn_example()
                    # raw_input()
                    if converted != sentence.text:
                        logger.critical("{sent} was not properly processed".format(sent=sentence))

        if args.pickle_output_file is not None:
            logger.info('Writing pickle file')
            with open(args.pickle_output_file, 'wb') as f:
                pickle.dump(docs, f)

        if args.output_file is not None:
            logger.info('Writing {} file to {}'.format(adapter_out.file_format.upper(), args.output_file))
            with open(args.output_file, 'w') as f:
                for doc in docs:
                    adapter_out.write_doc(doc, file_or_file_name=f)
        else:
            logger.info('Printing {} data'.format(adapter_out.file_format.upper()))
            for doc in docs:
                print adapter_out.doc_to_string(doc)


    try:
        main(_argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)
