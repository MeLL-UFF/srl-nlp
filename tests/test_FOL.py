"""
Test unit for the fol.py module
"""
from unittest import TestCase

from fol import FOL
from nose.tools import assert_raises


def test_someA_with_a_and():
    """some(A, and(b, a(c)))"""
    f1 = FOL('some(A, and(b, a(c)))').info
    l1 = ['some', ['A'], ['and', ['b'], ['a', ['c']]]]
    assert FOL._eq_predicate(f1, l1)


def test_someA_allBwith_a_and():
    """some(A, all(B, and(b, a(c))))"""
    f1 = FOL('some(A, all(B, and(b, a(c))))').info
    l1 = ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1, l1)


class TestFOL(TestCase):
    # def test_parse(self):
    #     self.fail()

    def test_is_operator(self):
        in_list = ['and', 'AND', 'or', 'OR', 'Dumbledore']
        out_list = [True, False, True, False, False]

        for in_test, out_test in zip(in_list, out_list):
            assert FOL.is_operator(in_test) == out_test

    def test_is_quantifier(self):
        in_list = ['and', 'AND', 'or', 'OR', 'Dumbledore']
        out_list = [True, False, True, False, False]

        for in_test, out_test in zip(in_list, out_list):
            assert FOL.is_operator(in_test) == out_test

    def test_is_special(self):
        in_list = ['and', 'AND', 'or', 'OR', 'not', 'Not', 'NOT', 'Dumbledore']
        out_list = [True, False, True, False, True, False, False, False]

        for in_test, out_test in zip(in_list, out_list):
            assert FOL.is_special(in_test) == out_test

    def test__parse_aux(self):
        in_list = [['and', '(', 'A', ',', 'B', ')'],
                   ['or', '(', 'A', ',', 'and', '(', 'B', ',', 'C', ')', ')'],
                   ['some', '(', 'A', ',', 'all', '(', 'B', ',', 'and', '(', 'b', ',', 'a', '(', 'c', ')', ')', ')',
                    ')'],
                   []]
        out_list = [['and', ['A'], ['B']],
                    ['or', ['A'], ['and', ['B'], ['C']]],
                    ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]],
                    None]
        fol = FOL()
        for in_test, out_test in zip(in_list, out_list):
            output = fol._parse_aux(in_test, [])
            assert output == out_test, (output, out_test)

    def test__split(self):
        out_list = [['and', '(', 'A', ',', 'B', ')'],
                    ['or', '(', 'A', ',', 'and', '(', 'B', ',', 'C', ')', ')'],
                    ['or', '(', 'A', ',', 'and', '(', '\'Bubles are magical\'', ',', 'C', ')', ')'],
                    ['or', '(', 'A', ',', 'and', '(', r"'Bubles are \'awe\\nsome\''", ',', 'C', ')', ')'],
                    ['some', '(', 'A', ',', 'and', '(', 'b', ',', 'a', '(', 'c', ')', ')', ')']]
        in_list = list(map(lambda x: ''.join(x) + '.', out_list))

        in_list.append('   some  (  A  , all  (  B     , and(     b, a(  c) \n)\t)    ) ')
        out_list.append(
            ['some', '(', 'A', ',', 'all', '(', 'B', ',', 'and', '(', 'b', ',', 'a', '(', 'c', ')', ')', ')', ')'])
        for in_test, out_test in zip(in_list, out_list):
            output = FOL._split(in_test)
            assert output == out_test, (output, out_test)

    def test__parse_aux_exception(self):
        # TODO more exception tests
        in_list = [
            ['some', '(', 'A', ',', ' ', ',', 'and', '(', 'b', ',', 'a', '(', 'c', ')', ')', ')']
        ]
        for in_test in in_list:
            assert_raises(FOL._parse_aux(in_test, []))

    def test_skolemize(self):
        # TODO test with for alls
        in_list = [
            ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]],
            ['fol', ['1'], ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]],
            # some(A,and(4,all(B,a(4)))) -> all(B,and(4,a(4)))
            ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]],
            ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['B']]]]],
        ]
        out_list = [
            ['and', ['b'], ['a', ['c']]],
            ['and', ['b'], ['a', ['c']]],
            ['all', ['B'], ['and', ['b'], ['a', ['c']]]],
            ['all', ['B'], ['and', ['b'], ['a', ['B']]]],  # TODO this b should be changed
        ]

        for in_test, test in zip(in_list, out_list):
            f = FOL()
            f.info = in_test
            f.skolemize()
            output = f.info
            assert output == test, (output, test)

    def test_convert2PrenexForm(self):
        in_list = [
            # some(A,and(b,some(B,a(c)))) -> some(A,some(B,and(b,a(c))))
            ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]],
            # fol(1, some(A,and(b,some(B,a(c))))) -> some(A,some(B,and(b,a(c))))
            ['fol', ['1'], ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]],
            ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]],
            # some(A, and (b, all(B, a(b)))) -> some(A, all(B, and (4, a(4))))
            ['some', ['A'], ['and', ['b'], ['all', ['B'], ['some', ['C'], ['a', ['C']]]]]],
        ]
        out_list = [
            ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]],
            ['fol', ['1'], ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]]],
            ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]],
            ['some', ['A'], ['all', ['B'], ['some', ['C'], ['and', ['b'], ['a', ['C']]]]]],
        ]
        for in_test, test in zip(in_list, out_list):
            f = FOL()
            f.info = in_test
            f.convert2PrenexForm()
            output = f.info
            assert output == test, (output, test)

    # def test__push_operand(self):
    #     self.fail()

    def test__push_quantifiers(self):
        in_list = [
            ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
        ]
        out_list = [
            ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]]
        ]

        for in_test, out_test in zip(in_list, out_list):
            output = FOL._push_quantifiers(in_test)
            assert output == out_test, (output, out_test)

    def test__push_negation(self):
        # TODO add more tests
        in_list = [
            # not(some(A,and(b,some(B, a(c))))) -> all(A, or(not(b),all(B, not(a(c)))))
            ['not', ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]],
            # some(A,and(b,not(some(B,a(4))))) -> some(A,and(b,all(B,not(a(4))))))
            ['some', ['A'], ['and', ['b'], ['not', ['some', ['B'], ['a', ['c']]]]]],
            ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]],
        ]
        out_list = [
            ['all', ['A'], ['or', ['not', ['b']], ['all', ['B'], ['not', ['a', ['c']]]]]],
            ['some', ['A'], ['and', ['b'], ['all', ['B'], ['not', ['a', ['c']]]]]],
            ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]],
        ]

        for in_test, out_test in zip(in_list, out_list):
            output = FOL._push_negation(in_test)
            assert output == out_test, (output, out_test)

    # def test__negate(self):
    #     self.fail()
    #
    # def test__str_aux(self):
    #     self.fail()
    #
    # def test__eq_predicate(self):
    #     self.fail()
