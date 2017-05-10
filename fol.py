class FOL:
    NOT    = 'not'
    EXISTS = 'some'
    AND    = 'and'
    OR     = 'or'
    ALL    = 'all'
    def __init__(self, text):
        self.info = FOL.parse(text.strip())

    @staticmethod
    def parse(text):
        return FOL._parse_aux(FOL._split(text)) if len(text) > 0 else []

    @staticmethod
    def _parse_aux(queue):
        aux = []
        balance = 0
        predicate = []
        if len(queue) == 0:
            return None
        while len(queue) > 0:
            term = queue.pop(0)
            if term == '(':
                predicate.append(FOL._parse_aux(queue))
                balance += 1
            elif term == ')':
                if len(predicate) < 2:
                    queue.insert(0, term)
                else:
                    balance -= 1
                break
            elif term == ',':
                if len(predicate) < 2:
                    queue.insert(0, term)
                    break
                predicate.append(FOL._parse_aux(queue))
            else:
                predicate.insert(0, term.strip().replace("'", "\\'").replace('"', '\\"'))
        assert balance == 0, 'Error parsing FOL'
        return predicate


    @staticmethod
    def _split(text, sep = ',()', include_sep = True, filter = ['', '.']):
        queue = []
        token = ''
        for l in text:
            if l in sep:
                if len(token) > 0: queue.append(token.strip())
                token = ''
                if include_sep: queue.append(l)
            else:
                token += l
        if len(token) > 0: queue.append(token.strip())
        if queue[-1] == '.': queue.pop()
        return queue


    def skolemize(self, has_header = True, removeForAlls = False, ignore=['@placeholder']):
        '''This method converts the FOL to its Skolem form.
        has_header: defines if the first predicate should be ignored
        removesForAlls: tells if we should eliminate the universal quantifiers too
        ignore: provides a list of constants or varibles that should be left unchanged (even if their quantifiers are )
        '''
        self.convert2PrenexForm()
        constants = []
        term = self.info[2] if has_header else self.info
        #Find all existential variables
        oldTerm = None
        while term[0] == FOL.EXISTS or term[0] == FOL.ALL:
            if removeForAlls or not term[0] == FOL.ALL:
                if term[1][0] not in ignore:
                    constants.extend(term[1])
                if oldTerm:
                    oldTerm[2] = term[2]
                else:
                    oldTerm = term
                    self.info = term
            term = term[2]

        frontier = [term]
        #search to replace every existential variable by a constant
        while len(frontier) > 0:
            child = frontier.pop()
            if len(child) < 2:
                if child[0]  in constants:
                    child[0] = 'c%s' %constants.index(child[0]) #TODO universal constants
            else:
                frontier.extend(child[1:])


    def convert2PrenexForm(self, header = 'fol'):
        if self.info[0] == header:
            term = self.info[2]
        else:
            term = self.info
        if len(term) < 2:
            return term
        term = FOL._pushNegation(term)
        term = FOL._pushQuantifiers(term)
        if self.info[0] == header:
            self.info[2] = term
        else:
            self.info = term

    @staticmethod
    def _pushQuantifiers(term):
        if len(term) < 2:
            return term
        if term[0] != FOL.ALL and term[0] != FOL.EXISTS:
            for pos, child in enumerate(term[1:]):
                if child[0] == FOL.ALL or child[0] == FOL.EXISTS:
                    quantTail = child[2]
                    child[2] = term
                    term[1+pos] = quantTail
                    term = child
                    break
        for pos, child in enumerate(term[1:]):
            term[1+pos] = FOL._pushQuantifiers(child)
        return term

    #tries to strip all quantifiers and operations from negation
    @staticmethod
    def _pushNegation(term):
        if len(term) < 2:
            return term
        if term[0] == FOL.NOT:
            term = FOL._negate(term[1])
        for pos, child in enumerate(term[1:]):
            term[1+pos] = FOL._pushNegation(child)
        return term


    @staticmethod
    def _negate(term):
        if term[0] == FOL.NOT:
            return term[1] # not(not(x)) = x
        if term[0] == FOL.AND:
            return [FOL.OR]+ map(FOL._negate, term[1:])  # not(and(x,y)) = or(not(x), not(y))
        if term[0] == FOL.OR:
            return [FOL.AND]+ map(FOL._negate, term[1:]) # not(or(x,y)) = and(not(x), not(y))
        if term[0] == FOL.ALL:
            return [FOL.EXISTS, term[1]]+ map(FOL._negate, term[2:]) # not(all(X,y)) = exists(X, not(y))
        if term[0] == FOL.EXISTS:
            return [FOL.ALL, term[1]] + map(FOL._negate, term[2:])    # not(exists(X,y)) = all(X, not(y))
        return [FOL.NOT, term]


    @staticmethod
    def toLF(fol, available_ids = None):
        return ""

    def _has_left_child(self):
        return self.info != None and len(self.info) > 1 and self.info[1] != None

    def _has_right_child(self):
        return self.info != None and len(self.info) > 2 and self.info[2] != None

    #TODO: it supposes that there are at most 2 variables in a term
    def _has_children(self):
        return self._has_left_child(self) or self._has_right_child(self)

    @staticmethod
    def _str_aux(info):
        if info == None:
            out = ''
        else:
            out = info[0]
            if self._has_children():
                out += '(%s)' % ','.join(map(self._str_aux, info[1:]))
        return out

    @staticmethod
    def _str_lf(info, and_t = ',', or_t = ';', header = 'fol'):
        if info == None:
            out = ''
        else:
            if info[0] == FOL.AND:
                out = '%s' % and_t.join(map(FOL._str_lf, info[1:]))
            elif info[0] == FOL.OR:
                out = '%s' % or_t.join(map(FOL._str_lf, info[1:]))
            elif info[0] == FOL.ALL or info[0] == FOL.EXISTS:
                out = FOL._str_lf(info[2])
            else:
                out = info[0]
                if len(info) > 1:
                    out += '(%s)' % ','.join(map(FOL._str_lf, info[1:]))
        return out

    def str_lf(self, and_t = ',', or_t = ';', header = 'fol'):
        info = self.info[2] if self.info == header else self.info
        return FOL._str_lf(info, and_t, or_t, header)+ '.'

    def __repr__(self):
        return FOL._str_aux(self.info) + '.'