#!/bin/env python

import logging
import pickle
from copy import copy

from srl_nlp.framenet.framenet import Description

logger = logging.getLogger(__name__)


class Document:
    aggregations = {'SENTENCE_AVERAGE': lambda l: float(sum(l)) / len(l)}

    def __init__(self, id, corpus='', corpusID=None, name='', desc='', sentences=None, **params):
        self.id = id
        self.desc = desc
        self.name = name
        self.corpus = corpus
        self.corpusID = corpusID
        if sentences is None:
            self.sentences = []
        else:
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
        lsents = sorted(self.sentences, key=get_id)
        rsents = sorted(other.sentences, key=get_id)
        if len(lsents) != len(rsents):
            return False
        for i, (lsent, rsent) in enumerate(zip(lsents, rsents)):
            eq = (rsent == lsent)
            evaluation = 'EQ' if eq else 'DIFF'
            logger.debug('[{i}/{total}] {eq} sentences: \n\t{lsent} and \n\t {rsent}' \
                         .format(i=i, total=len(rsents), lsent=lsent, rsent=rsent, eq=evaluation))
            if not eq:
                return False
        # TODO
        return True

    def _similar(self, other, fn=None, aggregation='SENTENCE_AVERAGE'):
        aggregation = self.aggregations[aggregation]
        if self.id != other.id:
            logger.debug('Diff ids: \'{id1}\' and \'{id2}\''.format(id1=self.id,
                                                                    id2=other.id))
            return False
        if len(self.sentences) != len(other.sentences):
            logger.debug(('Diff quantity of sentences: id(\'{id1}\'):{len1}'
                          'and id(\'{id2}\'):{len2}').format(id1=self.id, id2=other.id),
                         len1=len(self), len2=len(other))
            return False
        get_id = lambda sent: sent.id
        lsents = sorted(self.sentences, key=get_id)
        rsents = sorted(other.sentences, key=get_id)
        similarities = []
        for i, (lsent, rsent) in enumerate(zip(lsents, rsents)):
            sim = (rsents._similar(lsents))  # TODO
            logger.debug('[{i}/{total}] Sim:{sim:6.2} sentences: \n\t{lsent} and \n\t {rsent}' \
                         .format(i=i, total=len(rsents), lsent=lsent,
                                 rsent=rsent, sim=sim * 100))
            similarities.append(sim)
        # TODO
        return aggregation(similarities)

    def __str__(self):
        return 'Doc[{id}]:\'{desc}\' ({sent} sentences)'.format(id=self.id, desc=self.desc, sent=len(self.sentences))

    def __repr__(self):
        return str(self)


class Sentence:
    def __init__(self, id, text, annotation_sets=[], **params):
        self.id = id
        self.text = text
        self.params = params
        if (annotation_sets is None):
            self.annotation_sets = []
        else:
            self.annotation_sets = annotation_sets

    def __len__(self):
        return len(self.text)

    def __iter__(self):
        return self.annotation_sets.__iter__()

    def __getitem__(self, item):
        """If item is an annotation we return a slice of the sentence.
           If item is an slice, just slice the text.

           Returns substring of self.text"""
        # logger.debug('Slicing sentence {sent}, item \'{item}\''.format(sent = self, item = item))
        if isinstance(item, Annotation):
            return self[item.start:(item.end + 1)]
        elif isinstance(item, slice):
            return self.text[item]
        # logger.debug('Item is confusing ')
        return

    def _order_by_pos(self, anno_list, in_place):
        # TODO documentation
        key = lambda anno: (anno.start, -anno.end)
        if in_place:
            anno_list.sort(key=key)
        else:
            return sorted(anno_list, key=key)

    def _handle_annotation(self, content, anno, escapeHTML, Label):
        anno = copy(anno)
        # Base case
        if isinstance(content, str):
            ltext = content[0:anno.start]
            fex_cont = content[anno.start:anno.end + 1]
            rtext = content[anno.end + 1:]
            logger.debug('Recursion end: [\'{}\',\'{}\',\'{}\']'.format(ltext, fex_cont, rtext))
            fex = Label(content=[fex_cont], name=anno.name, escapeHTML=escapeHTML)
            # logger.debug('Fex.content: \'{}\'\n{}'.format(fex.content,fex))
            # logger.warning('Fex \'{}\', len(Fex) = {}'. format(fex, len(fex)))
            result = []
            if len(ltext) > 0:
                result.append(ltext)
            result.append(fex)
            if len(rtext) > 0:
                result.append(rtext)
            return result
        # Keep digging into the content
        pos = 0
        # logger.debug('Annotation content: "{}"'.format(content))
        while anno.start >= len(content[pos]):
            anno.start = anno.start - len(content[pos])
            anno.end = anno.end - len(content[pos])
            pos = pos + 1
            assert anno.start >= 0
        subcont = content[pos]
        # logger.debug('Subcontent: {}'.format(subcont))
        result = self._handle_annotation(subcont, anno, escapeHTML, Label)
        # logger.debug('Result:{}'.format(result))
        # logger.debug('Instance of subclass: {}'.format(subcont.__class__))
        if isinstance(subcont, Label):
            pass
        elif isinstance(subcont, str):
            content[pos:pos + 1] = result
        else:
            raise Exception('Invalid type {}'.format(subcont.__class__))
        # logger.debug('!!!!Content:{}'.format(content))
        return content

    def get_fn_example(self, escapeHTML=False, **attribs):
        """
        Returns the equivalent FrameNet example of the given sentence
        """
        anno_list = []
        for anno_set in self.annotation_sets:
            if anno_set.is_frame():
                for layer in anno_set:
                    for anno in layer:
                        if anno.is_fe():
                            anno_list.append(anno)
        self._order_by_pos(anno_list, in_place=True)
        logger.debug(map(lambda x: (x.start, x.end, x.name), anno_list))
        # raw_input()
        logger.debug('Sentence "{sent}", fex: "{fex}"'.format(sent=self, fex=anno_list))
        content = [self.text]
        logger.debug('Sentence ({})'.format(self.text))
        if len(anno_list) > 0:
            for anno in anno_list:
                logger.debug('Fex "{fex}"'.format(fex=anno))
                content = self._handle_annotation(content, anno, escapeHTML=escapeHTML, Label=Description.FEeXample)
                logger.debug('Content:{}'.format(content))
                logger.debug('Anno slice: \'{}\''.format(self[anno]))
        example = Description.EXample(content=content,
                                      escapeHTML=False, **attribs)
        return example

    def __eq__(self, other):
        if self.id != other.id or self.text != other.text:
            return False
        if len(self.annotation_sets) != len(other.annotation_sets):
            return False
        # TODO
        return True

    def _similar(self, other, fn=None):
        if self.id != other.id or self.text != other.text:
            return False
        # TODO
        return True

    def __str__(self):
        return 'Sentence[{id}]:\'{desc}\''.format(id=self.id, desc=self.text)

    def __repr__(self):
        return str(self)


class AnnotationSet:
    def __init__(self, id, frameID=None, frameName=None, luID=None,
                 luName=None, status=None, layers=None, **params):
        self.id = id
        self.frameID = frameID
        self.frameName = frameName
        self.luID = luID
        self.luName = luName
        self.status = status
        self.params = params
        if layers is None:
            self.layers = []
        else:
            self.layers = layers

    def is_frame(self):
        return (self.frameName is not None) or (self.frameID is not None)

    def get_fes(self):
        return list(filter(Annotation.is_fe, self.layers))

    def __iter__(self):
        return self.layers.__iter__()

    def __len__(self):
        return len(self.layers)

    def __hash__(self):
        return (self.id, self.frameID, self.frameName,
                self.luID, self.luName, self.status).__hash__()

    def __eq__(self, other):
        if self.id != other.id or \
                self.frameID != other.frameID or \
                self.frameName != other.frameName or \
                self.luID != other.luID or \
                self.luName != other.luName:
            return False
        for anno1, anno2 in zip(self.layers, other.layers):
            # TODO
            pass
        return True

    def _similar(self, other, fn=None):
        # TODO
        return True

    def __str__(self):
        params_str = str(self.params) if len(self.params) > 0 else ''
        if self.is_frame():
            return '<Frame \'{fr}\'>{anno}{params}</Frame>'.format(fr=self.frameName, anno=str(self.layers)[1:],
                                                                   params=params_str)
        else:
            return '<AnnoSet>{anno}{params}</AnnoSet>'.format(anno=str(self.layers)[1:], params=params_str)

    def __repr__(self):
        # TODO
        return str(self)


class Layer:
    def __init__(self, name, rank=None, annotations=None, **params):
        self.name = name
        self.rank = rank
        if annotations is None:
            self.annotations = []
        else:
            self.annotations = annotations
        self.params = params

    def __iter__(self):
        return self.annotations.__iter__()

    def __eq__(self, other):
        if self.id != other.id or \
                self.name != other.name or \
                self.rank != other.rank:
            return False
        for anno1, anno2 in zip(self.annotations, other.annotations):
            # TODO
            pass
        return True

    def _similar(self, other, fn=None):
        # TODO
        return True

    def __str__(self):
        # params_str = str(self.params) if len(self.params) > 0 else ''
        labels = '\n'.join(map(str, self.annotations))
        return '<Layer \'{name}\'>{anno}</Layer>'.format(name=self.name, anno=labels)

    def __repr__(self):
        # TODO
        return str(self)


class Annotation:
    def __init__(self, start=0, end=0, itype=None, name=None, **params):
        self.start = int(start)
        self.end = int(end)
        self.itype = itype
        self.name = name
        self.params = params
        pass

    def is_fe(self):
        return self.name is not None  # TODO

    def is_subannotation(self, other):
        # if self.itype is not None or other.itype is not None:
        #    return False
        try:
            if self.start >= other.start and self.end <= other.end:
                return True
        except (TypeError, AttributeError) as e:
            pass
        return False

    def __len__(self):
        return self.end - self.start + 1

    def is_null(self):
        """
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
        """
        return self.itype is not None

    def __hash__(self):
        return (self.start, self.end, self.name).__hash__()

    def __eq__(self, other):
        return (self.start == self.start and
                self.end == self.end and
                self.name == self.name)  # TODO

    def _similar(self, other):
        # TODO
        return True

    def __str__(self):
        if self.is_fe():
            return '<fe \'{fe}\'>({start},{end})</fe>'.format(fe=self.name, start=self.start, end=self.end)
        else:
            return '<label>({start},{end})</label>'.format(fe=self.name, start=self.start, end=self.end)

    def __repr__(self):
        return str(self)
