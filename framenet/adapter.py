#!/bin/env python

from sys                      import argv
from srl_nlp.logger_config    import config_logger, add_logger_args
from srl_nlp.framenet.corpus  import *
import xml.etree.ElementTree  as XMLTree
import pickle

import logging
import argparse

logger = logging.getLogger(__name__)

class FNXMLAdapter:
    _TAG_PREFIX='{http://framenet.icsi.berkeley.edu}'
    def __init__(self, **params):
        self.params = params
        #TODO

    def _parse_document(self, xml_node):
        doc_args = {}
        paragraphs = {}
        logger.debug('ROOT tag:{tag}'.format(tag = xml_node.tag))
        for xml_child in xml_node:
            logger.debug('Child tag:{tag}'.format(tag = xml_child.tag))
            if xml_child.tag == self._TAG_PREFIX+'header':
                try:
                    xml_corpus_header    = xml_child.getchildren()[0]
                    xml_doc_header       = xml_corpus_header.getchildren()[0]
                    doc_args['corpus']   = xml_corpus_header.attrib.get('name', '')
                    doc_args['corpusID'] = xml_corpus_header.attrib.get('ID', '')
                    doc_args['id']       = xml_doc_header.attrib.get('ID', '')
                    doc_args['desc']     = xml_doc_header.attrib.get('description', '')
                    doc_args['name']     = xml_doc_header.attrib.get('name', '')
                except IndexError as e:
                    logger.error(e)
            elif self._TAG_PREFIX+'sentence' == xml_child.tag:
                xml_sent = xml_child
                parNo    = int(xml_sent.attrib['paragNo'])
                aPos     = int(xml_sent.attrib['aPos'])
                par      = paragraphs.get((parNo, aPos), [])
                par.append(self._parse_sentence(xml_sent))
                paragraphs[(parNo, aPos)]  = par
        assert len(doc_args) > 0, 'Not enough information to parse a Document'

        ordering = lambda (x,_): x
        ord_par = [par for _, par in sorted(paragraphs.items(), key = ordering)]
        sentences = []
        for par in ord_par:
            sentences.extend(par)
        doc_args['sentences'] = sentences
        doc = Document(**doc_args)
        return doc

    def _parse_sentence(self, xml_node, **params):
        logger.debug('Sentence({}):{}'.format(xml_node.tag, xml_node.attrib))
        assert xml_node.tag == self._TAG_PREFIX+'sentence'
        id = xml_node.attrib['ID']
        text = None
        annotation_sets = []
        for child in xml_node:
            if child.tag == self._TAG_PREFIX+'text':
                text = child.text
            if child.tag == self._TAG_PREFIX+'annotationSets':
                for anno_set in child:
                    annotation_sets.append(self._parse_annoset(anno_set))
        sentence = Sentence(id = id, text = text, annotation_sets = annotation_sets)
        return sentence

    def _parse_annoset(self, xml_node, **params):
        logger.debug(xml_node.tag)
        assert self._TAG_PREFIX+'annotationSet' == xml_node.tag
        annos = []
        for layer in xml_node:
            assert self._TAG_PREFIX+'layer' == layer.tag
            try:
                annos.append(self._parse_annotation(layer))
            except IndexError:
                continue

        id        = xml_node.get('ID', None)
        frameID   = xml_node.get('frameRef', None)
        frameName = xml_node.get('frameName', None)
        luID      = xml_node.get('luID', None)
        luName    = xml_node.get('luName', None)
        status    = xml_node.get('status', None)

        anno_set = AnnotationSet(id = id,
                                 frameID = frameID,
                                 frameName = frameName,
                                 luID = luID,
                                 luName = luName,
                                 status = status,
                                 annotations = annos,
                                 **params)  
        return anno_set

    def _parse_annotation(self, xml_node, **params):
        #TODO change for the layer level
        logger.debug('Annotation[{}]:{}'.format(xml_node.tag, xml_node.attrib))
        assert self._TAG_PREFIX+'layer' == xml_node.tag
        children = xml_node.getchildren()
        if len(children) > 0:
            label = children.getchildren()[0]
            logger.debug('{lab}:{attr}'.format(lab= label.tag, attr = label.attrib))
            name  = label.attrib['name']
            itype = label.attrib.get('itype', None)
            if itype:
                anno  = Annotation(name = name, itype = itype, **params)
            else:
                start = label.attrib['start']
                end   = label.attrib['end']
                anno  = Annotation(start = start, end = end, name = name, **params)
            return anno
        else:
            err = IndexError('Empty layer id({id})'.format(id = xml_node.attrib.get('ID','?')))
            logger.warning(err)
            raise err

    def parseXML(self, xml_item):
        '''
        Returns a list of documents
        '''
        if isinstance(xml_item, XMLTree.Element):
            root = xml_item
        else:
            root = XMLTree.parse(xml_item).getroot()
        return [self._parse_document(root)]

    def doc2XML(self, doc, xml_file = None):
        root = self._doc2XML(doc)
        tree = XMLTree.ElementTree(root)
        if xml_file != None:
            tree.write(xml_file)
        else:
            return XMLTree.dump(tree)

    def _doc2XML(self, doc):
        root = XMLTree.Element('fullTextAnnotation')
        root.attrib = {'xmlns':'http://framenet.icsi.berkeley.edu',
                       'xmlns:xsi':'http://www.w3.org/2001/XMLSchema-instance'}
        header = XMLTree.Element('fullTextAnnotation')
        header.attrib = {'xmlns':'http://framenet.icsi.berkeley.edu',
                       'xmlns:xsi':'http://www.w3.org/2001/XMLSchema-instance'}
        corpus = XMLTree.Element('corpus')
        corpus.attrib = {'description':'',
                         'name':doc.corpus, 'ID':doc.corpusID}
        document = XMLTree.Element('document')
        document.attrib = {'description':doc.desc,
                           'name':doc.name, 'ID':doc.ID}
        corpus.append(doc)
        header.append(corpus)
        root.append(header)

        for sent in doc.sentences:
            xml_doc.append(self._sentence2XML(sent), corpID = doc.corpusID, docID = doc.id)
        
        return root

    def _sentence2XML(self, sent, corpID, docID):
        xml_sent = XMLTree.Element('sentence')
        xml_sent.attrib = {'corpID': corpID,
                            'docID': docID,
                            #'sentNo': None,
                            #'paragNo': None,
                            #'aPos': None,
                            'ID': sent.id}
        xml_text = XMLTree.Element('text')
        xml_text.text = sent.text
        xml_sent.append(xml_text)
        for annoSet in sent.annotation_sets: #TODO
            xml_sent.append(self._anno_set2XML(annoSet))

    def _anno_set2XML(self, annoset):
        xml_anno_set = XMLTree.Element('annotationSet')
        xml_anno_set.attrib = {'ID':annoset.id}

        for anno in annoset:
            sentence= XMLTree.Element('')
            text= XMLTree.Element('')
            sentence= XMLTree.Element('')

######------------------------------

class SemEval07XMLAdapter(FNXMLAdapter):
    _TAG_PREFIX=''
    def __init__(self, **params):
        self.params = params
        #TODO

    def _parse_document(self, xml_node):
        id = xml_node.attrib['ID']
        desc = xml_node.attrib['description']
        paragraphs = {}
        for xml_par_list in xml_node:
            assert 'paragraphs' == xml_par_list.tag
            for xml_par in xml_par_list:
                #logger.debug('Paragraph:\'{}\''.format(xml_par.tag))
                assert 'paragraph' == xml_par.tag
                sentences = []
                for xml_sents in xml_par:
                    assert 'sentences' == xml_sents.tag
                    for xml_sent in xml_sents:
                        sentences.append(self._parse_sentence(xml_sent))
                pos = int(xml_par.attrib['documentOrder'])
                sentences.extend(paragraphs.get(pos, []))
                paragraphs[pos] = sentences

        ordering = lambda (x,_): x
        sentences = []
        for _, sent_list in sorted(paragraphs.items(), key = ordering):
            sentences.extend(sent_list)
        doc = Document(id = id, desc = desc,sentences = sentences)
        return doc

    def _parse_annoset(self, xml_node, **params):
        logger.debug(xml_node.tag)
        assert 'annotationSet' == xml_node.tag
        annos = []
        for layers in xml_node:
            for layer in layers:
                #TODO assert layer has type layer
                try:
                    annos.append(self._parse_annotation(layer))
                except IndexError:
                    continue

        id        = xml_node.get('ID', None)
        frameID   = xml_node.get('frameRef', None)
        frameName = xml_node.get('frameName', None)
        luID      = xml_node.get('lexUnitRef', None)
        luName    = xml_node.get('luName', None)
        status    = xml_node.get('status', None)

        anno_set = AnnotationSet(id = id,
                                 frameID = frameID,
                                 frameName = frameName,
                                 luID = luID,
                                 luName = luName,
                                 status = status,
                                 annotations = annos,
                                 **params)  
        return anno_set

    def parseXML(self, xml_item):
        '''
        Returns a list of documents
        '''
        if isinstance(xml_item, XMLTree.Element):
            root = xml_item
        else:
            root = XMLTree.parse(xml_item).getroot()
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

def parse_args(argv = argv, add_logger_args = lambda x: None):
    parser = argparse.ArgumentParser(description = 'Parses the semeval07 and framenet format into Documment')
    parser.add_argument('input_file', help = 'File to be parsed')
    parser.add_argument('-o', '--output_file', help = 'File to write the pickle serialization')
    parser.add_argument("-p","--parser", choices=PARSERS_AVAILABLE.keys(), help = 'Parser for the appropriate kind of file')
    add_logger_args(parser)
    args = parser.parse_args(argv[1:])
    return args


def main(argv):
    args = parse_args(argv, add_logger_args)
    config_logger(args)
    #file_path = '/home/bcarvalho/DataSets/semeval07/trial/xml/ElectionVictory_anno.xml'
    logger.info('Loading XML file')
    with open(args.input_file, 'r') as f:
        tree = XMLTree.parse(f)
    logger.info('XML tree ready')
    root = tree.getroot()

    adapter = PARSERS_AVAILABLE.get(args.parser, SemEval07XMLAdapter)()

    logger.info('Parsing XML tree')
    docs = adapter.parseXML(root)
    logger.info('Done parsing XML tree')

    if args.output_file != None:
        logger.info('Writing pickle file')
        with open(args.output_file, 'wb') as f:
            pickle.dump(docs,f)
        

if __name__ == '__main__':
    try:
        main(argv)
    except KeyboardInterrupt:
        logger.info('Halted by the user')
    except OSError as e:
        logger.critical('Problem reading/writing files')
        logger.exception(e)