'''
Test unit for the logicalform.py module
'''
from unittest import TestCase

from nose.tools import assert_equals
from srl_nlp.fol import FOL
from srl_nlp.logicalform import LF


class TestLF(TestCase):
    def test_split(self):
        in_list = [
            ['some', ['A'], ['and', ['pernammary', ['A']], ['not', ['some', ['B'], ['some', ['C'],
                                                                                    ['and', ['r1Theme', ['B'], ['C']],
                                                                                     ['and', ['r1Actor', ['B'], ['A']],
                                                                                      ['and', ['v1like', ['B']],
                                                                                       ['n1cookie', ['C']]]]]]]]]],
        ]
        out_list = [
            ['all', ['B'], ['all', ['C'], ['and', ['pernammary', ['c0']], ['or', ['not', ['r1Theme', ['B'], ['C']]],
                                                                           ['not', ['r1Actor', ['B'], ['c0']]],
                                                                           ['not', ['v1like', ['B']]],
                                                                           ['not', ['n1cookie', ['C']]]]]]]
        ]

        for in_test, test in zip(in_list, out_list):
            f = FOL()
            f.info = in_test
            lf = LF(f)
            output = lf.info
            assert_equals(output, test)

    def test_iterterms(self):
        self.fail()

    def test_get_pred(self):
        self.fail()

    def test_set_pred(self):
        self.fail()

    def test_has_pred(self):
        self.fail()

    def test_isleaf(self):
        self.fail()

    def test__repr_aux(self):
        self.fail()
