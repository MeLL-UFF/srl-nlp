import logging
from copy import deepcopy as copy

logger = logging.getLogger(__name__)


class FOL:
    NOT = 'not'
    EXISTS = 'some'
    AND = 'and'
    OR = 'or'
    ALL = 'all'

    def __init__(self, text=None, *extra_args):
        if text is None:
            self.info = []
        else:
            self.info = FOL.parse(text.strip(), *extra_args)

    @staticmethod
    def parse(text, *extra_args):
        """Returns a tree representing the terms in the fol
        Params:
            text: a text in the format: '<clause>.'
                <clause> := <id>(<clause>+)|<id>
                <id> := [A-Za-z0-9_#]+ | '.*'
            extra_args: terms to be added to the non-special (not quantifier or operand) predicates
        """
        args = map(lambda x: [str(x)], extra_args)
        return FOL._parse_aux(FOL._split(text), args) if len(text) > 0 else []

    @staticmethod
    def is_operator(predicate):
        """Returns True if the string predicate is an operand.
            is_operator(str) -> boolean value
        """
        return predicate in [FOL.AND, FOL.NOT, FOL.OR]

    @staticmethod
    def is_quantifier(predicate):
        """Returns True if the string predicate is a quantifier.
            is_quantifier(str) -> boolean value
        """
        return predicate in [FOL.ALL, FOL.EXISTS]

    @staticmethod
    def is_special(predicate):
        """Returns True if the string predicate is a quantifier or operator.
            is_special(str) -> boolean value
            is_special(str) := is_quantifier(str) or is_quantifier(str)
        """
        return predicate in [FOL.AND, FOL.NOT, FOL.OR, FOL.ALL, FOL.EXISTS]

    @staticmethod
    def _parse_aux(queue, extra_args):
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
                assert len(predicate) == 0, 'Wrong parsing ' + term + "@" + str(predicate)
                predicate.insert(0, term.strip())
        assert balance == 0, 'Error parsing FOL'
        return predicate

    @staticmethod
    def _split(text, sep=',()', str_marker="'", include_sep=True):
        """Breaks the raw text into useful tokens.
        """
        queue = []
        token = ''
        str_flag = False
        escape_flag = False
        text = text.strip()
        for character in text:
            if character == str_marker and not escape_flag:
                if str_flag:
                    queue.append(str_marker + token + str_marker)
                str_flag = not str_flag
                token = ''
                continue
            if character == '\\':
                escape_flag = not escape_flag
            else:
                escape_flag = False
            if str_flag:
                token = token + character
                continue
            if character in sep:
                token = token.strip()
                if len(token) > 0:
                    queue.append(token)
                if include_sep:
                    queue.append(character)
                token = ''
            else:
                token += character
        if len(token) > 0:
            queue.append(token.strip())
        if queue[-1] == '.':
            queue.pop()

        # print "\n@@:", text
        assert not str_flag, 'Parsing FOL: String not terminated'
        # print ')):', queue
        return queue

    def skolemize(self, header='fol', removeForAlls=False, constant_prefix='c', ignore=('@placeholder',),
                  add_quotm=False):
        """This method converts the FOL to its Skolem form.

        Args:
            header: defines if the first predicate should be ignored
            removeForAlls: tells if we should eliminate the universal quantifiers too
            constant_prefix: prefix of the constant tokens that replaced the variables
            ignore: provides a list of constants or varibles that should be left unchanged (even if their quantifiers are )
            add_quotm: bool. Adds quotation marks to all terms that are not constants
        """
        self.convert2PrenexForm()
        constants = []
        if self.info[0] == header:
            term = self.info[-1]
        else:
            term = self.info
        # Find all existential variables
        old_term = None
        while term[0] == FOL.EXISTS or term[0] == FOL.ALL:  # if the predicate is a quantifier
            if removeForAlls or term[0] == FOL.EXISTS:  # if I should remove this quantifier
                if term[1][0] not in ignore:  # store variable to update it in the next predicates
                    constants.extend(term[1])
                if old_term:
                    old_term[2] = term[2]
                else:
                    self.info = term[2]
            else:
                old_term = term
            term = term[2]
        frontier = [term]
        # search to replace every existential variable by a constant
        while len(frontier) > 0:
            child = frontier.pop()
            logger.debug('PrenexForm child: %s', child)
            if len(child) < 2:
                if child[0] in constants:
                    child[0] = '{}{}'.format(constant_prefix, constants.index(child[0]))
                elif add_quotm:
                    child[0] = "'{}'".format(child[0])
            else:
                frontier.extend(child[1:])

    def convert2PrenexForm(self, header='fol'):
        """Changes its structure to represent a FOL in Prenex Form

            A FOL f is in Prenex Form when all its quantifiers are in the left most side.
            In this implementation we also 'push' all negations to the non-special predicates.
            We do so in order to be able to 'move' the quantifiers without changing satisfiability.
        """
        if self.info[0] == header:
            term = self.info[-1]
        else:
            term = self.info
        if len(term) < 2:
            return term
        term = FOL._push_negation(term)
        term = FOL._push_quantifiers(term)
        if self.info[0] == header:
            self.info[-1] = term
        else:
            self.info = term

    @staticmethod
    def _push_operand(term, op):
        """
        Move all the operands of the same kind of op to the 'leaves'.
        This operation respect negation. It does not move any operator across a negation.

        Args:
            term: the term to be changed, usually something like 'fol.info'
            op: the kind of operator to be pushed: FOL.AND or FOL.OR

        Returns:
            Boolean value stating if a change was made or not.

        """
        if FOL.is_quantifier(term[0]):
            for child in (term[1:]):
                FOL._push_operand(child, op)  # , aggregate=aggregate)
        if FOL.is_operator(term[0]):
            for child in term[1:]:
                FOL._push_operand(child, op)  # , aggregate=aggregate)
            for pos, child in enumerate(term[1:]):
                if FOL.is_operator(child[0]):
                    if child[0] == FOL.NOT:
                        if term[0] == child[0]:
                            # if aggregate:
                            #     term.pop(pos + 1)
                            #     term.extend(child[1:])
                            #     FOL._push_operand(term, op, aggregate=aggregate)
                            return False
                        else:
                            if term[0] == op:
                                term.pop(pos + 1)
                                siblings = term[1:]
                                del (term[:])  # empty this list without losing references
                                term.append(child[0])
                                term.extend([[op, i] + copy(siblings) for i in child[1:]])
                                if FOL._push_operand(term, op):  # , aggregate=aggregate):
                                    FOL._push_operand(term, op)  # , aggregate=aggregate)
                                return True
                            else:
                                if FOL._push_operand(child, op):  # , aggregate=aggregate):
                                    FOL._push_operand(term, op)  # , aggregate=aggregate)
        return False

    def push_operand(self, op):  # , aggregate=aggregate)
        """
        Move all the operands of the same kind of op to the 'leaves'.
        This operation respects negation. It does not move any operator across a negation.

        Args:
            op: the kind of operator to be pushed: FOL.AND or FOL.OR

        Returns:
            Nothing, change in-place.
        """
        FOL._push_operand(self.info, op)  # , aggregate=aggregate)

    @staticmethod
    def _push_quantifiers(term, root=None):
        """Moves all quantifiers to the beginning of the formula."""
        current = None
        try:
            if root is None:
                root = term
            if len(term) >= 2:
                frontier = [term]
                while len(frontier):
                    current = frontier.pop()
                    for pos, child in enumerate(current[1:]):
                        if FOL.is_special(child[0]):
                            if FOL.is_quantifier(child[0]):
                                current[1 + pos] = child[2]
                                child[2] = root[2]
                                root[2] = child
                                root = child
                                frontier.append(child)
                                break
                            else:
                                frontier.append(child)
        except IndexError as e:
            logger.error('_push_quantifiers: %s', current, exc_info=True)
            raise e
        return term

    @staticmethod
    def _push_negation(term):
        """Moves all negation to the 'leaf' terms."""
        if len(term) < 2:
            return term
        if term[0] == FOL.NOT:
            term = FOL._negate(term[1])
        for pos, child in enumerate(term[1:]):
            term[1 + pos] = FOL._push_negation(child)
        return term

    @staticmethod
    def _negate(term):
        """Return the negation of a term.

        Uses some simple properties (like DeMorgan) to translate the negation of a formula to the negation of its terms
        """
        if term[0] == FOL.NOT:
            return term[1]  # not(not(x)) = x
        if term[0] == FOL.AND:
            return [FOL.OR] + map(FOL._negate, term[1:])  # not(and(x,y)) = or(not(x), not(y))
        if term[0] == FOL.OR:
            return [FOL.AND] + map(FOL._negate, term[1:])  # not(or(x,y)) = and(not(x), not(y))
        if term[0] == FOL.ALL:
            return [FOL.EXISTS, term[1]] + map(FOL._negate, term[2:])  # not(all(X,y)) = exists(X, not(y))
        if term[0] == FOL.EXISTS:
            return [FOL.ALL, term[1]] + map(FOL._negate, term[2:])  # not(exists(X,y)) = all(X, not(y))
        return [FOL.NOT, term]

    @staticmethod
    def _str_aux(info):
        if info is None or len(info) < 1:
            out = ''
        else:
            out = info[0]
            if len(info) > 1:
                out += '(%s)' % ','.join(map(FOL._str_aux, info[1:]))
        return out

    def __eq__(self, other):
        if self is None or other is None:
            if self is None and other is None:
                return True
            else:
                return False
        return FOL.equals_predicate(self.info, other.info)

    def __hash__(self):
        return repr(self).__hash__()

    @staticmethod
    def equals_predicate(l_list, r_list):
        """
        Checks if predicates are equivalent:
            l_list == r_list

        Args:
            l_list: left hand predicate
            r_list: right hand predicate

        Returns:
            Boolean value.

        """
        if len(l_list) != len(r_list):
            return False

        for l_term, r_term in zip(l_list, r_list):
            if str(l_term) and str(r_term):
                if l_term != r_term:
                    return False
            else:
                return FOL.equals_predicate(l_term, r_term)
        return True

    def __repr__(self):
        return FOL._str_aux(self.info) + '.'
