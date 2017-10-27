from copy import deepcopy as copy
from sys  import stderr
from fol import FOL
import logging

logger = logging.getLogger(__name__)

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
                logger.debug('FOL: %s', fol)
                fol.convert2PrenexForm()
                logger.debug('PRENEX FOL: %s', fol)
                #print '*', fol
                fol.skolemize(**kargs)
                logger.debug('SKOLEMIZED FOL: %s', fol)
                fol.push_operand(FOL.OR)
                self.info = fol.info
                logger.debug('LF: %s', self)
            except AttributeError as e:
                raise Exception('Not a valid FOL%s' %fol)
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

    def __eq__(self, other):
        if self is None or other is None:
            if self is None and other is None:
                return True
            else:
                return False
        return FOL._eq_predicate(self.info, other.info)

    def __repr__(self, and_t = ',', or_t = ';', supress_not = False):
        '''Return a str representation of the LF
        '''
        if self.info == None or len(self.info) < 1:
            out = ''
        else:
            term = self.info
            out = LF._repr_aux(term, and_t, or_t, supress_not) + '.'
        return out
