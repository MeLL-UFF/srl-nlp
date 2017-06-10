from copy import deepcopy as copy

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
    def is_operator(predicate):
        return predicate in [FOL.AND, FOL.NOT, FOL.OR]

    @staticmethod
    def is_quantifier(predicate):
        return predicate in [FOL.ALL, FOL.EXISTS]

    @staticmethod
    def is_special(predicate):
        return predicate in [FOL.AND, FOL.NOT, FOL.OR, FOL.ALL, FOL.EXISTS]

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

    def skolemize(self, has_header = True, removeForAlls = False, ignore=['@placeholder'], **kargs):
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
    def _push_operand(term, op, header = 'fol'):
        #self.convert2PrenexForm()
        if FOL.is_quantifier(term[0]):
            for child in(term[1:]):
                FOL._push_operand(child, op, header)
            return term
        if FOL.is_operator(term[0]):
            for child in term[1:]:
                FOL._push_operand(child, op, header)
            for pos, child in enumerate(term[1:]):
                #print 'child is operator', FOL.is_operator(child[0])
                #print 'term is OP', term[0] == op
                #print 'grandchildren has operator', sum(map(lambda x: x[0] == FOL.AND, child[1:]))
                if FOL.is_operator(child[0]):
                    if(child[0] != FOL.NOT):
                        if term[0] == child[0]:
                            term.pop(pos+1)
                            term.extend(child[1:])
                            FOL._push_operand(term, op, header)
                            continue
                        else:
                            if term[0] == op:
                                term.pop(pos+1)
                                siblings = term[1:]
                                del(term[:]) #empty this list without losing references
                                term.append(child[0])
                                term.extend([[op,i] + copy(siblings) for i in child[1:]])
                                FOL._push_operand(term, op, header)
                                continue

    def push_operand(self, op, header = 'fol'):
        FOL._push_operand(self.info, op, header)

    @staticmethod
    def _pushQuantifiers(term):
        '''Moves all quantifiers to the begining of the formula.'''
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
        '''Moves all negation to the 'leaf' terms.'''
        if len(term) < 2:
            return term
        if term[0] == FOL.NOT:
            term = FOL._negate(term[1])
        for pos, child in enumerate(term[1:]):
            term[1+pos] = FOL._pushNegation(child)
        return term

    @staticmethod
    def _negate(term):
        '''Return the negation of a term.

        Uses some simple properties (like DeMorgan) to translate the negation of a formula to the negation of its terms
        '''
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
    def _str_aux(info):
        if info == None or len(info) < 1:
            out = ''
        else:
            out = info[0]
            if len(info) > 1:
                out += '(%s)' % ','.join(map(FOL._str_aux, info[1:]))
        return out  

    def __repr__(self):
        return FOL._str_aux(self.info) + '.'


class LF:
    def __init__(self, *args, **kargs):
        '''Creates a new Logical Formula
        LF(fol) -> lf

        The first parameter must be the source to be usedto generate the LF.
        If it is a FOL, you can specify the argument 'header' to eliminate the header if it is there
        '''
        header = kargs.get('header',None)
        if header and args[0][0] == header:
            fol = copy(args[0][-1])
        else:
            fol= copy(args[0])
        if len(args) > 0:
            try:
                fol= copy(args[0])
                fol.convert2PrenexForm()
                fol.skolemize(**kargs)
                fol.push_operand(FOL.OR)
                self.info = fol.info
            except AttributeError as e:
                print 'Not a valid FOL'
        else:
            self.info = []

    @staticmethod
    def _repr_aux(term, and_t, or_t, supress_not, header):
        try:
            parser = lambda term: LF._repr_aux(term, and_t, or_t, supress_not, header)
            if supress_not:
                term = filter(lambda x: x[0] != FOL.NOT, term)
            if term[0] == FOL.AND:
                out = '%s' % and_t.join(map(parser, term[1:]))
            elif term[0] == FOL.OR:
                out = '(%s)' % or_t.join(map(parser, term[1:]))
            elif (term[0] == FOL.ALL or term[0] == FOL.EXISTS) and len(term) > 2:
                out = parser(term[2])
            else:
                out = term[0]
                if len(term) > 1:
                    out += '(%s)' % ','.join(map(parser, term[1:]))
        except Exception as e:
            print e
            raise Exception('Ill-formed FOL:%s' %term)
        return out


    def __repr__(self, and_t = ',', or_t = ';', supress_not = False, header = 'fol'):
        '''Return a str representation of the LF
        '''
        if self.info == None or len(self.info) < 1:
            out = ''
        else:
            term = self.info
            out = LF._repr_aux(term, and_t, or_t, supress_not, header) + '.'
        return out
