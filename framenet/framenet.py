class Lexeme:
    def __init__(self, name = '', pos = '', breakBefore = True, headWord = True, text = ''):
        self.name        = name
        self.pos         = pos
        self.breakBefore = breakBefore
        self.headWord    = headWord
        self.text        = text


class LexicalUnit:
    def __init__(self, name, pos = '', status = '', definition = '',
                 anottation = (0,0), lexeme = None):
        self.name       = name
        self.pos        = pos
        self.status     = status
        self.definition = definition
        self.anottation = anottation
        self.lexeme     = lexeme


class Description:

    class FEName:
        def __init__(self, name, escapeHTML = False):
            self.name = name
            self.escapeHTML = escapeHTML

        def __eq__(self, other):
            return self.name == other.name

        def __hash__(self):
            return self.name.__hash__()

        def __str__(self, escapeHTML = None):
            if escapeHTML or self.escapeHTML:
                open_tag = '&lt;'
                close_tag = '&gt;'
            else:
                open_tag = '<'
                close_tag = '>'
            return '{open}fen{close}{name}{open}/fen{close}'.format(name    = self.name,
                                                                    open    = open_tag,
                                                                    close   = close_tag)
        def __repr__(self):
            return 'FEName(\'{name}\')'.format(name = self.name)

    class FEeXample:
        def __init__(self, name, description, escapeHTML = False, **args):
            self.name = name
            self.description = description
            self.escapeHTML = escapeHTML
            for k,v in args.iteritems():
                setattr(self, k, v)

        def __hash__(self):
            return (self.name, self.description).__hash__()

        def __str__(self, escapeHTML = None):

            if escapeHTML or self.escapeHTML:
                open_tag = '&lt;'
                close_tag = '&gt;'
            else:
                open_tag = '<'
                close_tag = '>'
            return '{open}fex name="{name}"{close}{desc}{open}/fex{close}'.format(open  = open_tag,
                                                                                  close = close_tag,
                                                                                  name  = self.name,
                                                                                  desc  = self.description)

        def __repr__(self):
            return 'FEeXample(\'{name}\':\'{desc}\')'.format(name= self.name,desc= self.description)

    class EXample:
        def __init__(self, escapeHTML = False):
            self.content = []
            self.escapeHTML = escapeHTML

        def __str__(self, escapeHTML = None):
            if escapeHTML or self.escapeHTML:
                return '&lt;ex&gt;%s&lt;/ex&gt;' %''.join(map(str, self.content))
            else:
                return '<ex>%s</ex>' % (''.join(map(str, self.content)))

        def __repr__(self):
            return 'Example(\'{desc}\')'.format(desc = self.content)

        def __hash__(self):
            return self.content.__hash__()

        def add_text(self, text):
            if len(self.content) and type(self.content[-1]) == str:
                self.content[-1] = self.content[-1]+text
            else:
                self.content.append(text)

        def add_element(self, element):
            self.content.append(element)

    class Special:
        def __init__(self, content, name = 'special', escapeHTML = False):
            self.name = name
            self.content = content
            self.escapeHTML = escapeHTML

        def __eq__(self, other):
            return self.name == other.name and self.content == other.content

        def __hash__(self):
            return (self.name, self.content).__hash__()

        def __str__(self, escapeHTML = None):
            if escapeHTML or self.escapeHTML:
                open_tag = '&lt;'
                close_tag = '&gt;'
            else:
                open_tag = '<'
                close_tag = '>'
            return '{open}{name}{close}{content}{open}/{name}{close}'.format(name    = self.name,
                                                                             content = self.content,
                                                                             open    = open_tag,
                                                                             close   = close_tag)

        def __repr__(self):
            return str(self)

    class T(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'t',escapeHTML)

    class M(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'m',escapeHTML)

    class Ment(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'ment',escapeHTML)

    class Gov(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'gov',escapeHTML)

    class EM(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'em',escapeHTML)

    class Supp(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'supp',escapeHTML)

    class Target(Special):
        def __str__(self, content, escapeHTML = False):
            super(content,'target',escapeHTML)


    def __init__(self, escapeHTML = False):
        self.fens = set()
        self.specials = set()
        self.content = []
        self.escapeHTML = escapeHTML

    def add_text(self, text):
        if len(self.content) and type(self.content[-1]) == str:
            self.content[-1] = self.content[-1]+text
        else:
            self.content.append(text)

    def add_element(self, element):
        self.content.append(element)
        if isinstance(element, Description.FEName):
            self.fens.add(element)
        elif isinstance(element, Description.Special):
            self.specials.add(element)

    def has_special_annotation(self):
        return len(self.specials) > 0

    def has_fe_annotation(self):
        return len(self.fens) > 0

    def __contains__(self, element):
        return element in self.fens or element in self.specials

    def __str__(self, escapeHTML = None):
        if escapeHTML or self.escapeHTML:
            return '&lt;def-root&gt;%s&lt;/def-root&gt;' % (''.join(map(str, self.content)))
        else:
            return '<def-root>%s</def-root>' % (''.join(map(str, self.content))) #TODO

    def __repr__(self):
        return str(self) #TODO


class Frame:

    class Element:
        def __init__(self, name = '', abbrev = '', definition = '',
                     fgColor = 'black', bgColor = 'white', isCore = True, semanticType = ''):
            self.name       = name
            self.abbrev     = abbrev
            self.definition = definition
            self.fgColor    = fgColor
            self.bgColor    = bgColor
            self.isCore     = isCore

        def __eq__(self, other):
            return self.name == other.name and self.abbrev == other.abbrev

        def __hash__(self):
            return (self.name, self.abbrev).__hash__()

        def __str__(self):
            #return '<Frame "%s" %s>' %(self.name, dir(self))
            return '<Frame Element "%s"[%s]>' %(self.name, self. abbrev)
        def __repr__(self):
            return str(self)

    class Relation:
        def __init__(self, name, frames = []):
            self.name   = name
            self.frames = frames

        def __str__(self):
            #return '<Frame "%s" %s>' %(self.name, dir(self))
            frame_names = map(lambda x: x.name, self.frames)
            return '%s: %s' %(self.name, str(frame_names)[1:-1])

        def __repr__(self):
            frame_names = map(lambda x: x.name, self.frames)
            return '{%d}' %str(frame_names)[1:-1]

        def __eq__(self, other):
            if type(other) == str:
                return self.name == other
            else:
                return self.name == other.name

    def __init__(self, name = '', description = '', coreFEs = [],
                 peripheralFEs = [], LUs = [], **relations):
        #TODO exceptions
        assert name != None #None type is not an acceptable name
        self.name          = name
        self.description   = description
        self.coreFEs       = coreFEs
        self.peripheralFEs = peripheralFEs
        self.LUs           = LUs
        self.relations     = relations

    def hasFEinCore(self, fe):
        '''Check if fe is in the frame as a core Frame Element'''
        return fe in self.coreFEs

    def hasFEnotInCore(self, fe):
        '''Check if fe is in the frame as a peripheral Frame Element'''
        return fe in self.peripheralFEs

    def __str__(self):
        #return '<Frame "%s" %s>' %(self.name, dir(self))
        return '<Frame "%s">' %self.name

    def _in_transitive_closure(self, relationName, other, investigated = []):
        '''Checks if a given frame, other, can be reached by a transitive closure of the relation.'''
        investigated.append(self)
        for relation in self.relations:
            if relation.name == relationName:
                if other in relation.frames:
                    return True
                else:
                    for child in relation.frames:
                        if not child in investigated:
                            if self._in_transitive_closure(relationName, other, investigated):
                                return True
        return False

    def in_transitive_closure(self, relationName, other):
        '''Checks if a given frame, other, can be reached by a transitive closure of the relation.'''
        return self._in_transitive_closure(relationName, other, [])


    def __hash__(self):
        return self.name.__hash__()

    def __eq__(self, other):
        eq_core = [ fe in other.coreFEs for fe in self.coreFEs] + \
                  [fe in self.coreFEs for fe in other.coreFEs]
        eq_peripheral = [ fe in other.peripheralFEs for fe in self.peripheralFEs] + \
                        [fe in self.peripheralFEs for fe in other.peripheralFEs]
        
        criteria = [self.name == other.name,] + eq_core + eq_peripheral
        return reduce(lambda x,y: x and y, criteria)

    def __repr__(self):
        return str(self)


class Net:
    def __init__(self, frames):
        '''FrameNet python API

        frames: A list of Frames or a dictionary where the names are the keys
        '''
        if type(frames) != dict: #if the user passes a list of frames instead of a dict then convert it
            frames = dict(zip(map(lambda x: x.name, frames), frames))
        self.frames = frames
        for frame in frames.itervalues():
            self._update_frame_references(frame)
        self._fes = dict()
        self._fes2frames = dict()
        for frame in self:    
            for fe in frame.coreFEs + frame.peripheralFEs:
                self._fes[fe.name]   = fe
                self._fes2frames[fe] = self._fes2frames.get(fe, [])
                self._fes2frames[fe].append(frame)

    def _update_frame_references(self, frame):
        '''Uses the self.frames dict to replace the strings representing Frames by actual Frames'''
        getRel = lambda x:  x if isinstance(x, Frame) else self.frames[x]
        for relation in frame.relations.itervalues():
            relation.frames = map(getRel, relation.frames)

    def __iter__(self):
        return self.frames.itervalues()

    def __getitem__(self, item):
        return self.frames[item]

    def __len__(self):
        '''Number of Frames in the Network'''
        return len(self.frames)

    def getFrameElement(self, name):
        '''Returns the Frames in the NetFrame by name'''
        return self._fes[name]

    def getFrameElementFrames(self, fe, coreFEs = True, peripheralFEs = True):
        '''Get all frames where the given fe is a Frame Element
        
        coreFEs: boolean value, if set True then Frames where fe is a core element will be returned
        peripheralFEs: boolean value, if set True then Frames where fe is a non-core element will be returned
        
        Returns a list of frames
        '''
        if not isinstance(fe, Frame.Element):
            fe = self._fes[fe]
        frames = self._fes2frames[fe]
        if (not coreFEs) or (not peripheralFEs):
            if (not coreFEs) and (not peripheralFEs):
                return []
            elif coreFEs:
                frame_filter = lambda frame: frame.hasFEinCore(fe)
            else:
                frame_filter = lambda frame: frame.hasFEnotInCore(fe)
            frames = filter(frame_filter, frames)
        return frames

    def str(self):
        return '<FrameNet, %d frames>' %len(self)

    def repr(self):
        return str(self)