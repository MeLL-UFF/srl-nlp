#!/bin/env python

from srl_nlp.logger_config   import config_logger, add_logger_args
import xml.etree.ElementTree as XMLTree

class AnnotationSet:
    def __init__(self, params):
        pass

    def parse_xml(xml):
        if True:#type of node is xml.node:
            pass
        else:
            pass

    def is_frame(self):
        return True

    def fe_count(self):
        return 0

    def get_fes(self):
        pass

class SemEval07Adapter:
    def __init__(self, params):
        '''params are the files locations and stuff'''
        self.name = ''
        self.id   = ''
        self.docs = []

    def _add_sentence(self, text, pos, semantics):
        pass

    def _add_document(self, id, description):
        pass

    def _add_paragraph(self, id, documentOrder):
        pass

    def _add_annotation(self, text, pos, semantics):
        pass

    def load(self, sentence_list, pos_list, semantics_list):
        if len(sentence_list) == len(pos_list) == len(semantics_list):
            for i in zip(sentence_list, pos_list, semantics_list):
                pass
        else:
            raise Exception('Invalid sentence, pos, or semantic lists')

    def loadXML(self, xml):
        tree = XMLTree.parse(xml).getroot()
        c_name = tree.attrib['name']
        c_id = tree.attrib['ID']
        for doc_list in tree:
            for document in doc_list:
                self._add_document(document.attrib['ID'],document.attrib['description'])
                for par_list in document:
                    for paragraph in par_list:
                        self._add_paragraph(paragraph.attrib['ID'], int(paragraph.attrib['documentOrder']))
                        for sent_list in paragraph:
                            for sentence in sent_list:
                                self._add_sentence('')
                                for anno_sets in sentence: #TODO
                                    self._add_annotation(AnnotationSet.parse_xml(anno_sets))


    def dump(self, file, **params):
        file.write(prefix)
        for doc in documents:
            self._dump_documents(file)
        file.write(suffix)

    def _dump_documents(self, file, **params):
        file.write(prefix)
        for doc in documents:
            self._dump_paragraphs(file)
        file.write(suffix)

    def _dump_paragraphs(self, file, **params):
        file.write(prefix)
        for doc in documents:
            self._dump_sentences(file)
        file.write(suffix)

    def _dump_sentences(self, file, **params):
        file.write(prefix)
        for doc in documents:
            self._dump_sentences(file)
        file.write(suffix)