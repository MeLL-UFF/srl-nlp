from copy import deepcopy as copy
from sys  import stderr

class FOL:
    NOT    = 'not'
    EXISTS = 'some'
    AND    = 'and'
    OR     = 'or'
    ALL    = 'all'
    def __init__(self, text, *extra_args):
        self.info = FOL.parse(text.strip(), *extra_args)

    @staticmethod
    def parse(text, *extra_args):
        args = map(lambda x: [str(x)], extra_args)
        #print "@@", text
        return FOL._parse_aux(FOL._split(text), args) if len(text) > 0 else []

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
    def _parse_aux(queue, extra_args):
        aux = []
        balance = 0
        predicate = []
        if len(queue) == 0:
            return None
        while len(queue) > 0:
            term = queue.pop(0)
            if term == '(':
                if not FOL.is_special(predicate[0]):
                    predicate.extend(extra_args)
                predicate.append(FOL._parse_aux(queue, extra_args))
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
                predicate.append(FOL._parse_aux(queue, extra_args))
            else:
                assert len(predicate) == 0, 'Wrong parsing '+ term + "@"+str(predicate)
                predicate.insert(0, term.strip())
        assert balance == 0, 'Error parsing FOL'
        return predicate


    @staticmethod
    def _split(text, sep = ',()', str_marker="'", include_sep = True):
        queue = []
        token = ''
        str_flag = False
        escape_flag = False
        for l in text:
            if l == str_marker and not escape_flag:
                if str_flag:
                    queue.append(str_marker+token+str_marker)
                str_flag = not str_flag
                token = ''
                continue
            if l == '\\':
                escape_flag = not escape_flag
            else:
                escape_flag = False
            if str_flag:
                token += l
                continue
            if l in sep:
                if len(token) > 0: queue.append(token.strip())
                token = ''
                if include_sep: queue.append(l)
            else:
                token += l
        if len(token) > 0: queue.append(token.strip())
        if queue[-1] == '.': queue.pop()

        #print "\n@@:", text
        assert not str_flag, 'Parsing FOL: String not terminated'
        #print ')):', queue
        return queue

    def skolemize(self, header = 'fol', removeForAlls = False, ignore=['@placeholder'], **kargs):
        '''This method converts the FOL to its Skolem form.

        has_header: defines if the first predicate should be ignored
        removesForAlls: tells if we should eliminate the universal quantifiers too
        ignore: provides a list of constants or varibles that should be left unchanged (even if their quantifiers are )
        '''
        self.convert2PrenexForm()
        constants = []
        if self.info[0] == header:
            term = self.info[-1]
        else:
            term = self.info
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
            term = self.info[-1]
        else:
            term = self.info
        if len(term) < 2:
            return term
        term = FOL._pushNegation(term)
        term = FOL._pushQuantifiers(term)
        if self.info[0] == header:
            self.info[-1] = term
        else:
            self.info = term

    @staticmethod
    def _push_operand(term, op):
        #self.convert2PrenexForm()
        if FOL.is_quantifier(term[0]):
            for child in(term[1:]):
                FOL._push_operand(child, op)
        if FOL.is_operator(term[0]):
            for child in term[1:]:
                FOL._push_operand(child, op)
            for pos, child in enumerate(term[1:]):
                #print 'child is operator', FOL.is_operator(child[0])
                #print 'term is OP', term[0] == op
                #print 'grandchildren has operator', sum(map(lambda x: x[0] == FOL.AND, child[1:]))
                if FOL.is_operator(child[0]):
                    if(child[0] != FOL.NOT):
                        if term[0] == child[0]:
                            term.pop(pos+1)
                            term.extend(child[1:])
                            FOL._push_operand(term, op)
                            return False
                        else:
                            if term[0] == op:
                                term.pop(pos+1)
                                siblings = term[1:]
                                del(term[:]) #empty this list without losing references
                                term.append(child[0])
                                term.extend([[op,i] + copy(siblings) for i in child[1:]])
                                if FOL._push_operand(term, op):
                                    FOL._push_operand(term, op)
                                return True
                            else:
                                if FOL._push_operand(child, op):
                                    FOL._push_operand(term, op)
        return False

    def push_operand(self, op):
        FOL._push_operand(self.info, op)

    @staticmethod
    def _pushQuantifiers(term, root = None):
        '''Moves all quantifiers to the begining of the formula.'''
        try:
            if root == None:
                root = term
            if len(term) >= 2:
                frontier = [term]
                while len(frontier):
                    current = frontier.pop()
                    for pos, child in enumerate(current[1:]):
                        if child[0] == FOL.EXISTS or child[0] == FOL.ALL:
                            current[1+pos] = child[2]
                            child[2] = root[2]
                            root[2] = child
                            root = child
                            frontier.append(child)
                            break
                        else:
                            frontier.append(child)
        except Exception as e:
            #print '>',tmp
            raw_input()
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
        if len(args) > 0:
            header = kargs.get('header',None)
            fol = copy(args[0])
            if header and args[0].info[0] == header:
                fol.info = fol.info[-1]
            #    print "*"
            #print ">", header, fol
            try:
                #print '\n-', fol
                fol.convert2PrenexForm()
                #print '*', fol
                fol.skolemize(**kargs)
                fol.push_operand(FOL.OR)
                self.info = fol.info
            except AttributeError as e:
                raise Exception('Not a valid FOL')
            #print '&',fol
            #raw_input()
        else:
            self.info = []

    def split(self):
        '''Split the clause into many smaller clauses'''
        frontier = [self.info]
        out = []
        while len(frontier) > 0:
            root = frontier.pop()
            while FOL.is_quantifier(root[0]):
                assert len(root) > 1, 'Invalid FOL'
                root = root[-1]
            #print '##' if root[0] == FOL.AND else ('xx'+str(root[0]))
            if root[0] == FOL.AND:
                frontier.extend(root[1:])
            else:
                lf = LF()
                lf.info = copy(root)
                out.append(lf)
        return out

    @staticmethod
    def _repr_aux(term, and_t, or_t, supress_not):
        try:
            parser = lambda term: LF._repr_aux(term, and_t, or_t, supress_not)
            if supress_not:
                term = filter(lambda x: x[0] != FOL.NOT, term)
            if term[0] == FOL.AND:
                out = '%s' % and_t.join(map(parser, term[1:]))
            elif term[0] == FOL.OR:
                out = '(%s)' % or_t.join(map(parser, term[1:]))
            #elif (term[0] == FOL.ALL or term[0] == FOL.EXISTS) and len(term) > 2:
            #    out = parser(term[-1])
            else:
                out = term[0]
                if len(term) > 1:
                    out += '(%s)' % ','.join(map(parser, term[1:]))
        except Exception as e:
            print >> stderr, e
            raise Exception('Ill-formed FOL:%s' %term)
        return out


    def __repr__(self, and_t = ',', or_t = ';', supress_not = False):
        '''Return a str representation of the LF
        '''
        if self.info == None or len(self.info) < 1:
            out = ''
        else:
            term = self.info
            out = LF._repr_aux(term, and_t, or_t, supress_not) + '.'
        return out
