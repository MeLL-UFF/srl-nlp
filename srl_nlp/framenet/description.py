import collections
import logging
import re
from typing import Dict, Union, List, Iterator

logger = logging.getLogger(__name__)


class Label(collections.Iterable):
    name = 'l'
    shortname = 'Label'

    def __init__(self, content=None, escape_html=False, **attribs):
        # type: (List[Union[Label,str]], bool, Dict[str,str]) -> None
        if content is None:
            self.content = []
        else:
            self.content = content
        self.attribs = attribs
        self.escape_html = escape_html

    def __str__(self, escape_html=False):
        attr = ''.join(['%s="%s"' % item for item in self.attribs.items()])
        if len(attr) > 0:
            attr = ' ' + attr
        if escape_html or self.escape_html:
            return '&lt;{name}{attr}&gt;{content}&lt;/{name}&gt;' \
                .format(name=self.name,
                        attr=attr,
                        content=''.join(map(str, self.content)))
        else:
            return '<{name}{attr}>{content}</{name}>' \
                .format(name=self.name,
                        attr=attr,
                        content=''.join(map(str, self.content)))

    def __len__(self):
        return sum(map(len, self.content))

    def __getitem__(self, item):
        return self.content[item]

    def __setitem__(self, item, val):
        logger.debug('Val %s' % val)
        logger.debug('Content "%s"' % self.content)
        if isinstance(item, slice):
            self.content = self.content[:item.start] + val + self.content[item.stop:]
        else:
            self[item:item] = val

    def __iter__(self):
        # type: () -> Iterator[Label]
        return self.content.__iter__()

    def __repr__(self):
        attr = ''.join(['%s = "%s"' % item for item in self.attribs.items()])
        if len(attr) > 0:
            attr = ' ' + attr
        return '{name}(\'{desc}\'{attr})'.format(name=self.shortname,
                                                 attr=attr,
                                                 desc=''.join(map(str, self.content)))

    def __hash__(self):
        # return (self.name, tuple(self.content), tuple(self.attribs.items())).__hash__()
        return (self.name, tuple(self.attribs.items())).__hash__()

    def __eq__(self, other):
        if hasattr(other, 'content'):
            return all((self.name == other.name,
                        self.content == other.content,
                        self.attribs.items() == other.attribs.items()))
        return False

    def add_text(self, text):
        if len(self.content) and type(self.content[-1]) == str:
            self.content[-1] = self.content[-1] + text
        else:
            self.content.append(text)

    def add_element(self, element):
        self.content.append(element)

    def set_attribs(self, **attribs):
        self.attribs = attribs

    def str_no_annotation(self):
        out = ''
        try:
            for elem in self.content:
                if isinstance(elem, Label):
                    text = elem.str_no_annotation()
                else:
                    text = elem
                out = out + text
        except TypeError as e:
            logger.error(str(self))
            raise e
        return re.sub(r'\s', ' ', out)


class FEName(Label):
    name = 'fen'
    shortname = 'FEName'


class FEeXample(Label):
    name = 'fex'
    shortname = 'FEeXample'


class EXample(Label):
    name = 'ex'
    shortname = 'EXample'


class Special(Label):
    name = 'special'
    shortname = 'Special'


class T(Special):
    name = 't'
    shortname = 'Target'


class M(Special):
    name = 'm'


class Ment(Special):
    name = 'ment'


class Gov(Special):
    name = 'gov'


class EM(Special):
    name = 'em'


class Supp(Special):
    name = 'supp'


class Target(Special):
    name = 'target'


class Description(collections.Iterable):

    def __init__(self, escape_html=False):
        self.fens = set()
        self.specials = set()
        self.content = []
        self.escapeHTML = escape_html
        self.tags = dict()

    def add_text(self, text):
        if len(self.content) and type(self.content[-1]) == str:
            self.content[-1] = self.content[-1] + text
        else:
            self.content.append(text)

    def add_element(self, element):
        self.content.append(element)
        if isinstance(element, FEName):
            self.fens.add(element)
        elif isinstance(element, Special):
            self.specials.add(element)
        self.tags[element.name] = self.tags.get(element.name, [])
        self.tags[element.name].append(element)

    def get_elements(self, element_or_element_name):
        # type: (Union[str,Label, Label.__class__]) -> List[Union[str,Label]]
        """Returns a list of elements that match element or element_name"""
        if hasattr(element_or_element_name, 'name'):
            element_name = element_or_element_name.name
        else:
            element_name = element_or_element_name
        return self.tags.get(element_name, [])

    def has_special_annotation(self):
        return len(self.specials) > 0

    def has_fe_annotation(self):
        return len(self.fens) > 0

    def get_fens(self):
        return self.fens

    def __iter__(self):
        # type: () -> Iterator[Union[Label, str]]
        return self.content.__iter__()

    def __contains__(self, element):
        return element in self.fens or element in self.specials

    def __str__(self, escape_html=False):
        if escape_html or self.escapeHTML:
            return '&lt;def-root&gt;%s&lt;/def-root&gt;' % (''.join(map(str, self.content)))
        else:
            return '<def-root>%s</def-root>' % (''.join(map(str, self.content)))

    def __repr__(self):
        return str(self)
