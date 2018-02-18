#!/bin/env python

from srl_nlp.logger_config   import config_logger, add_logger_args
import xml.etree.ElementTree as XMLTree
import logging
from copy import copy, deepcopy as dcopy

logger = logging.getLogger(__name__)

class Document:
    aggregations = {'SENTENCE_AVERAGE': lambda l: float(sum(l))/len(l)}
    
    def __init__(self, id, corpus = '', corpusID = None, name = '', desc = '', sentences = [], **params):
        self.id = id
        self.desc = desc
        self.sentences = sentences
        self.params = params

    def add_sentence(self, sentence):
        self.sentences.append(sentence)

    def get_sentences(self):
        return self.sentences

    @staticmethod
    def loadPickle(file):
        adapter = pickle.load(file)
        return adapter

    def savePickle(self, file):
        pickle.dump(self, file)

    def __len__(self):
        return len(self.sentences)

    def __iter__(self):
        return self.sentences.__iter__()

    def __eq__(self, other):
        if self.id != other.id or self.desc != other.desc:
            return False
        if len(self.sentences) != len(other.sentences):
            return False
        get_id = lambda sent: sent.id
        lsents = sorted(self.sentences,  key = get_id)
        rsents = sorted(other.sentences, key = get_id)
        if len(lsents) != len(rsents):
            return False
        for i,(lsent,rsent) in enumerate(zip(lsents,rsents)):
            eq = (rsent == lsent)
            evaluation = 'EQ' if eq else 'DIFF'
            logger.debugger('[{i}/{total}] {eq} sentences: \n\t{lsent} and \n\t {rsent}'\
                                .format(i = i, total = len(rsents), lsent = lsent, rsent = rsent, eq = evaluation))
            if not eq:
                return False
        #TODO
        return True

    def _similar(self, other, fn = None, aggregation = 'SENTENCE_AVERAGE'):
        aggregation = self.aggregations[aggregation]
        if self.id != other.id:
            logger.debug('Diff ids: \'{id1}\' and \'{id2}\''.format(id1 = self.id,
                                                                    id2 = other.id))
            return False
        if len(self.sentences) != len(other.sentences):
            logger.debug(('Diff quantity of sentences: id(\'{id1}\'):{len1}'
                          'and id(\'{id2}\'):{len2}').format(id1  = self.id, id2    = other.id),
                                                            len1 = len(self), len2 = len(other))
            return False
        get_id = lambda sent: sent.id
        lsents = sorted(self.sentences,  key = get_id)
        rsents = sorted(other.sentences, key = get_id)
        similarities = []
        for i,(lsent,rsent) in enumerate(zip(lsents,rsents)):
            sim = (rsents._similar(lsents))
            evaluation = 'EQ' if eq else 'DIFF'
            logger.debugger('[{i}/{total}] Sim:{sim:6.2} sentences: \n\t{lsent} and \n\t {rsent}'\
                                .format(i = i, total = len(rsents), lsent = lsent,
                                        rsent = rsent, sim = sim*100))
            similarities.append(sim)
        #TODO
        return aggregation(similarities)

    def __str__(self):
        return 'Doc[{id}]:\'{desc}\' ({sent} sentences)'.format(id=self.id, desc = self.desc, sent= len(self.sentences))

    def __repr__(self):
        return str(self)

class Sentence:
    def __init__(self, id, text, annotation_sets = [], **params):
        self.id = id
        self.text = text
        self.params = params
        if(annotation_sets == None):
            self.annotation_sets = []
        else:
            self.annotation_sets = annotation_sets

    def __len__(self):
        return len(self.text)

    def __iter__(self):
        return self.annotation_sets.__iter__()

    def __eq__(self, other):
        if self.id != other.id or self.text != other.text:
            return False
        if len(self.annotation_sets) != len(other.annotation_sets):
            return False
        #TODO
        return True

    def _similar(self, other, fn = None):
        if self.id != other.id or self.text != other.text:
            return False
        #TODO
        return True

    def __str__(self):
        return 'Sentence[{id}]:\'{desc}\''.format(id=self.id, desc = self.text)

    def __repr__(self):
        return str(self)

class AnnotationSet:
    def __init__(self, id, frameID = None, frameName = None, luID = None,
                 luName = None, status = None, annotations = None, **params):
        self.id        = id
        self.frameID   = frameID
        self.frameName = frameName
        self.luID      = luID
        self.luName    = luName
        self.status    = status
        self.params    = params
        if(annotations == None):
            self.annotations = []
        else:
            self.annotations = annotations

    def is_frame(self):
        return (self.frameName != None) or (self.frameID != None)

    def get_fes(self):
        return list(filter(Annotation.is_fe, self.annotations))

    def __getitem__(self, item):
        '''If item is an annotation we return a slice of the sentence.
           If item is an slice, just slice the text.

           Returns substring of self.text'''
        if isinstance(item, Annotation):
            return self[item.start, item.end+1]
        elif isinstance(item, slice):
            return text[item.start, item.end]
        return 

    def __len__(self):
        return len(self.annotations)

    def __iter__(self):
        return self.annotations.__iter__()

    def __hash__(self):
        return (self.id, self.frameID, self.frameName, \
                self.luID, self.luName, self.status).__hash__()

    def __eq__(self, other):
        if self.id        == other.id        and \
           self.frameID   == other.frameID   and \
           self.frameName == other.frameName and \
           self.luID      == other.luID      and \
           self.luName    == other.luName:
            return False
        for anno1, anno2 in zip(self.annotations, other.annotations):
            #TODO
            pass
        return True

    def _similar(self, other, fn = None):
        #TODO
        return True

    def __str__(self):
        params_str = str(self.params) if len(self.params) > 0 else ''
        if self.is_frame():
            return '<Frame \'{fr}\'>{anno}{params}</Frame>'.format(fr = self.frameName, anno = str(self.annotations)[1:-1], params = params_str)
        else:
            return '<AnnoSet>{anno}{params}</AnnoSet>'.format(anno = str(self.annotations)[1:-1], params = params_str)

    def __repr__(self):
        #TODO
        return str(self)

class Annotation:
    def __init__(self, start = None, end = None, itype = None, name = None, **params):
        self.start = start
        self.end = end
        self.name = name
        self.params = params
        pass

    def is_fe(self):
        return self.name != None #TODO

    def is_null(self):
        '''
        Kinds of Null:
        Definite Null Instantiation (DNI):
            An FE that is missing from a sentence, but whose identity is
            understood from the context, e.g. Stephanie contributed $20, where
            RECIPIENT, a charitable organization, is the missing FE.
        Constructional Null Instantiation (CNI):
            An FE that is missing because the grammar of the sentence allows or
            requires an omission (e.g. the subject of an imperative, the agent
            of a passive verb).
        Indefinite Null Instantiation (INI):
            An FE that is missing whose identity is not retrievable, but whose
            type is usually know (e.g. the FE TOPIC in Bob and Sue would argue
            all day, exemplifying the QUARREL sense of the verb argue
        ??? (APos):
            ????
            '''
        return itype != None
    def __hash__(self):
        return (self.start, self.end, self.name).__hash__()

    def __eq__(self, other):
        return (self.start == self.start and \
                self.end   == self.end and \
                self.name  == self.name) #TODO

    def _similar(self, other):
        #TODO
        return True

    def __str__(self):
        if self.is_fe():
            return '<fe \'{fe}\'>({start},{end})</fe>'.format(fe = self.name, start = self.start, end = self.end)
        else:
            return '<label>({start},{end})</label>'.format(fe = self.name, start = self.start, end = self.end)

    def __repr__(self):
        return str(self)