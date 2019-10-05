import collections
import distance as _dist
import logging
from typing import Dict, Union, List, Iterator

from srl_nlp.framenet.description import Description

logger = logging.getLogger(__name__)


class Lexeme:
    def __init__(self, name='', pos='', break_before=True, head_word=True, text=''):
        self.name = name
        self.pos = pos
        self.breakBefore = break_before
        self.headWord = head_word
        self.text = text

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "{name}.{pos}".format(name=self.name, pos=self.pos)

    def __eq__(self, other):
        return self.name == other.name and self.pos.lower() == other.pos.lower()

    def __hash__(self):
        return (self.name, self.pos).__hash__()


class LexicalUnit:
    def __init__(self, name, pos='', status='', definition='',
                 annotation=(0, 0), idx=None, lexeme=None):
        self.name = name
        self.pos = pos
        self.status = status
        self.definition = definition
        self.annotation = annotation
        self.id = idx
        self.lexeme = lexeme

    def __repr__(self):
        return "LU('{name}.{pos}',{anot},{status})".format(name=self.name,
                                                           pos=self.pos,
                                                           anot=self.annotation,
                                                           status=self.status)

    def __str__(self):
        return "{name}.{pos}".format(name=self.name, pos=self.pos)

    def __eq__(self, other):
        return self.name == other.name and self.pos.lower() == other.pos.lower()

    def __hash__(self):
        return (self.name, self.pos).__hash__()


class Frame:

    def __init__(self, name='', description='', core_fes=None,
                 peripheral_fes=None, lus=None, idx=None, **relations):
        # type: (str, Union[str,Description], List[FrameElement], List[FrameElement], List[LexicalUnit], Union[int, str], Dict[str, FrameRelation]) -> None
        assert name is not None  # None type is not an acceptable name
        self.name = name
        self.description = description

        if core_fes is None:
            self.coreFEs = []
        else:
            self.coreFEs = core_fes

        if peripheral_fes is None:
            self.peripheralFEs = []
        else:
            self.peripheralFEs = peripheral_fes

        if lus is None:
            self.LUs = []
        else:
            self.LUs = lus

        self.id = idx
        self.relations = relations

    @property
    def fes(self):
        return self.coreFEs + self.peripheralFEs

    def is_core_fe(self, fe):
        """Check if fe is in the frame as a core Frame Element"""
        return fe in self.coreFEs

    def is_peripheral_fe(self, fe):
        """Check if fe is in the frame as a peripheral Frame Element"""
        return fe in self.peripheralFEs

    def __str__(self):
        # return '<Frame "%s" %s>' %(self.name, dir(self))
        return '<Frame "%s">' % self.name

    def _in_transitive_closure(self, relation_name, other, investigated=None):
        """Checks if a given frame, other, can be reached by a transitive closure of the relation."""
        if investigated is None:
            investigated = []
        investigated.append(self)
        for relation in self.relations.values():
            if relation.name == relation_name:
                if other in relation.frames:
                    return True
                else:
                    for child in relation.frames:
                        if child not in investigated:
                            if self._in_transitive_closure(relation_name, other, investigated):
                                return True
        return False

    def in_transitive_closure(self, relation_name, other):
        """
        Checks if a given frame, other, can be reached by a transitive closure of
        the relation.
        """
        return self._in_transitive_closure(relation_name, other)

    def __hash__(self):
        # The way that relation references works need this hash to point to the frame's name hash
        return self.name.__hash__()

    def __eq__(self, other):
        # type: (Union[str, Frame]) -> bool
        """
        Two Frames are equal if they have the same name, the same Core Frame Elements,
        and the same Peripheral Frame elements.

        Args:
            other:
        """
        if other is None:
            return self is None
        if isinstance(other, str):
            return self.name == other

        eq_core = [fe in other.coreFEs for fe in self.coreFEs] + \
                  [fe in self.coreFEs for fe in other.coreFEs]
        eq_peripheral = [fe in other.peripheralFEs for fe in self.peripheralFEs] + \
                        [fe in self.peripheralFEs for fe in other.peripheralFEs]

        criteria = [self.name == other.name, ] + eq_core + eq_peripheral

        return all(criteria)

    def __repr__(self):
        return str(self)


class FrameElement:
    def __init__(self, name='', abbrev='', definition='',
                 fg_color='black', bg_color='white', is_core=True, semantic_type='', idx=None):
        self.name = name
        self.abbrev = abbrev
        self.definition = definition  # type: Union[str, Description]
        self.fgColor = fg_color
        self.bgColor = bg_color
        self.isCore = is_core
        self.semanticType = semantic_type
        self.id = idx

    def __eq__(self, other):
        if type(other) == str:  # allow comparison with string
            other_name = other
        else:
            other_name = other.name
        return self.name == other_name  # and self.abbrev == other.abbrev

    def __hash__(self):
        return self.name.__hash__()
        # return (self.name, self.abbrev).__hash__()

    def __str__(self):
        # return '<Frame "%s" %s>' %(self.name, dir(self))
        return '<Frame Element "%s"[%s]>' % (self.name, self.abbrev)

    def __repr__(self):
        return str(self)


class FrameRelation(collections.Iterable):
    def __init__(self, name, frames=None):
        # type: (str, List[Frame]) -> None
        self.name = name
        if frames is None:
            self.frames = []
        else:
            self.frames = frames

    def __iter__(self):
        # type: () -> Iterator[Union[Frame, str]]
        return self.frames.__iter__()

    def __str__(self):
        # return '<Frame "%s" %s>' %(self.name, dir(self))
        frame_names = str([frame.name for frame in self.frames])
        return '%s: %s' % (self.name, frame_names[1:-1])

    def __repr__(self):
        frame_names = str([frame.name for frame in self.frames])
        return '{%s}' % frame_names[1:-1]

    def __eq__(self, other):
        if type(other) == str:
            return self.name == other
        else:
            return self.name == other.name


class FrameNet(collections.Iterable):
    _word_distances = {'levenshtein': lambda x, y: _dist.levenshtein(x, y, normalized=True),
                       # 'hamming': distance.hamming #the two strings must have the same length
                       'jaccard': _dist.jaccard,
                       'sorensen': _dist.sorensen}

    def __init__(self, frames):
        """
        FrameNet python API

        Args:
            frames: A list of Frames or a dictionary where the names are the keys
        """
        if type(frames) != dict:  # if the user passes a list of frames instead of a dict then convert it
            frames = {frame.name: frame for frame in frames}
        self.frames = frames  # type: Dict[str, Frame]
        self.framesByID = dict([(frame.id, frame) for frame in frames.values()])  # type: Dict[int, Frame]
        for frame in frames.values():
            self._update_frame_references(frame)
        self._fes = dict()
        self._fes2frames = dict()
        for frame in self:
            for fe in frame.coreFEs + frame.peripheralFEs:
                self._fes[fe.name] = self._fes.get(fe.name, fe)
                self._fes2frames[fe] = self._fes2frames.get(fe, [])
                self._fes2frames[fe].append(frame)

    def _update_frame_references(self, frame):
        """Uses the self.frames dict to replace the strings representing Frames by actual Frames"""

        def get_rel(frame_name):
            out_frame = self.frames.get(frame_name, None)
            if out_frame is not None:
                return out_frame
            else:
                logger.warning('Relation pointing to not existent Frame "{}"'.format(frame_name))

        for relation in frame.relations.values():
            relation.frames = [f for f in map(get_rel, relation.frames) if f is not None]

    def __iter__(self):
        return iter(self.frames.values())

    def __getitem__(self, item):
        """Returns the Frames in the FrameNet by name or id, if item is a name or integer
           If item is a slice, then:

           self[word:qtd] = self.getMostSimilarFrames(word, qtd)
           self[word:qtd:distance] = self.getMostSimilarFrames(word, qtd, distance = distance)
                distace is a string
           self[word:qtd:threshold] = self.getMostSimilarFrames(word, qtd, threshold = threshold)
                threshold is an float in the interval [0, 1.0]
        """
        if type(item) == slice:
            token = item.start
            limit = item.stop
            extra = item.step
            if extra:
                if type(extra) == str:
                    return self.get_most_similar_frames(token, limit, distance=extra)
                else:
                    return self.get_most_similar_frames(token, limit, threshold=extra)
            else:
                return self.get_most_similar_frames(token, limit)
        else:
            frame = self.frames.get(item, None)
            if frame is None:
                frame = self.framesByID.get(item, None)
                if frame is None:
                    raise KeyError(
                        '\'{item}\' is not a valid Frame name or id in this FrameNet version'.format(item=item))

            return frame

    def __contains__(self, key):
        if hasattr(key, 'name'):
            return key.name in self.frames
        else:
            return key in self.frames

    def get_most_similar_frames(self, word, qtd=3, threshold=1, distance='levenshtein'):
        """
        Return an ordered list of the most similar Frames using the specified distance:

        Implemented distances:
            'levenshtein' : the minimum number of single-character edits
            'jaccard'     : the size of the intersection divided by the size of
                            the union of two label sets
            'sorensen'    : (F1-score) 

        self.getMostSimilarFrames(str, int, float, string) ->
                    [ (socore1, Frame1), ..., (socoreN, FrameN)] # N <= qtd
        """
        top_values = []
        metric = self._word_distances[distance]

        def distance(obj):
            return metric(obj.name, word)

        for frame in self:
            score = min(map(distance, frame.LUs + [frame]))
            if score <= threshold:
                pos = 0
                for other_score, _ in top_values:
                    if score > other_score:
                        pos = pos + 1
                    else:
                        break
                top_values.insert(pos, (score, frame))
                if len(top_values) > qtd:
                    top_values.pop()
        return top_values

    def __len__(self):
        """Number of Frames in the Network"""
        return len(self.frames)

    @property
    def fe_names(self):
        """
        Returns: The list of all Frame Element names
        """
        return set(self._fes.keys())

    @property
    def frame_names(self):
        """
        Returns: The list of all Frame names
        """
        return set(self.frames.keys())

    def has_frame_element(self, name):
        """
        Returns:
            A boolean value indicating if there is a Frame Elements in the FrameNet with this name
            """
        return name in self._fes

    def get_frame_element(self, name):
        """
        Returns:
            The Frame Elements in the FrameNet by name
        """
        try:
            return self._fes[name]
        except KeyError:
            raise KeyError('\'{item}\' is not a valid Frame Element name in this FrameNet'.format(item=name))

    def get_frame_element_frames(self, fe, core_fes=True, peripheral_fes=True):
        """Get all frames where the given fe is a Frame Element
        
        coreFEs: boolean value, if set True then Frames where fe is a core element will be returned
        peripheralFEs: boolean value, if set True then Frames where fe is a non-core element will be returned
        
        Returns a list of frames
        """
        if not isinstance(fe, FrameElement):
            fe = self._fes[fe]
        frames = self._fes2frames[fe]
        if (not core_fes) or (not peripheral_fes):
            if (not core_fes) and (not peripheral_fes):
                return []
            elif core_fes:
                return filter(lambda frame: frame.is_core_fe(fe),
                              frames)
            else:
                return filter(lambda frame: frame.is_peripheral_fe(fe),
                              frames)
        return frames

    def __str__(self):
        return '<FrameNet, %d frames>' % len(self)

    def __repr__(self):
        return str(self)
