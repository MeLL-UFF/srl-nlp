#!/bin/env python

from srl_nlp.logger_config   import config_logger, add_logger_args
import xml.etree.ElementTree as XMLTree
import logging
from copy import copy, deepcopy as dcopy

logger = logging.getLogger(__name__)

class Corpus:
    #TODO
    pass

class Document:
    
    def __init__(self, id, desc, sentences = [], **params):
        self.id = id
        self.desc = desc
        self.sentences = sentences
        self.params = params

    def add_sentence(self, sentence):
        self.sentences.append(sentence)

    def get_sentences(self):
        return self.sentences

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

    def _similar(self, other, fn = None, aggregation = lambda l: float(sum(l))/len(l)):
        if self.id != other.id:
            return False
        if len(self.sentences) != len(other.sentences):
            return False
        get_id = lambda sent: sent.id
        lsents = sorted(self.sentences,  key = get_id)
        rsents = sorted(other.sentences, key = get_id)
        similarities = []
        for i,(lsent,rsent) in enumerate(zip(lsents,rsents)):
            sim = (rsents._similar(lsents))
            evaluation = 'EQ' if eq else 'DIFF'
            logger.debugger('[{i}/{total}] Sim:{sim:6.2} sentences: \n\t{lsent} and \n\t {rsent}'\
                                .format(i = i, total = len(rsents), lsent = lsent, rsent = rsent, sim = sim*100))
            similarities.append(sim)
        #TODO
        return aggregation(similarities)


class Sentence:
    def __init__(self, id, text, annotation_sets = [], **params):
        self.id = id
        self.text = text
        self.params = params
        if(annotation_sets == None):
            self.annotation_sets = []
        else:
            self.annotation_sets = annotation_sets

    def __eq__(self, other):
        #TODO
        return True

    def _similar(self, other, fn = None):
        #TODO
        return True


class AnnotationSet:
    def __init__(self, ID, frameID = None, frameName = None, luID = None,
                 luName = None, status = None, annotations = None, **params):
        self.ID        = ID
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

    def __hash__(self):
        return (self.ID, self.frameID, self.frameName, \
                self.luID, self.luName, self.status).__hash__()

    def __eq__(self, other):
        #TODO
        return True

    def _similar(self, other, fn = None):
        #TODO
        return True

    def __str__(self):
        params_str = str(self.params) if len(params) > 0 else ''
        if self.is_frame():
            return '<Frame \'{fr}\'>{anno}{params}</Frame>'.format(fr = self.name, anno = str(self.annotations)[1:-1], params = params_str)
        else:
            return '<AnnoSet>{anno}{params}</AnnoSet>'.format(anno = str(self.annotations)[1:-1], params = params_str)

    def __repr__(self):
        #TODO
        return str(self)



class Annotation:
    def __init__(self, start, end, name = None, **params):
        #TODO
        pass

    def is_fe(self):
        return self.name != None

    def __hash__(self):
        return (self.start, self.end, self.name).__hash__()

    def __eq__(self, other):
        #TODO
        return True

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