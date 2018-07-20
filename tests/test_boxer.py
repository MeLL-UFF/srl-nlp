'''
Test unit for the fol.py module
'''
from unittest import TestCase

from analysers.boxer import BoxerLocalAPI, BoxerWebAPI
from logical_representation.fol import FOL
# Local API
from nose.tools import assert_equals


class TestBoxerLocalAPI(TestCase):
    def test_parse_sentence_to_fol(self):
        sentence = 'John is running even faster this time around.'
        boxer = BoxerLocalAPI()
        fol = boxer.sentence2FOL(sentence)
        assert str(fol) == '[some(A,some(B,some(C,and(and(n1time(B),and(a1around(A),and(r1Theme(A,B),' + \
                           'pernamjohn(C)))),some(D,some(E,and(r1Time(E,B),and(r1even(E),and(a1faster(D),' + \
                           'and(r1Manner(E,D),and(r1Actor(E,C),v1run(E)))))))))))).]'

    def test_parse_sentence(self):
        sentence = 'John is running even faster this time around.'
        boxer = BoxerLocalAPI()
        parsed = boxer._parse_sentence(sentence).strip()
        expected = ("ccg(1,"
                    "\n rp(s:dcl,"
                    "\n  ba(s:dcl,"
                    "\n   lx(np, n,"
                    "\n    t(n, 'John', 'John', 'NNP', 'I-NP', 'I-PER')),"
                    "\n   fa(s:dcl\\np,"
                    "\n    t((s:dcl\\np)/(s:ng\\np), 'is', 'be', 'VBZ', 'I-VP', 'O'),"
                    "\n    ba(s:ng\\np,"
                    "\n     ba(s:ng\\np,"
                    "\n      t(s:ng\\np, 'running', 'run', 'VBG', 'I-VP', 'O'),"
                    "\n      fa((s:ng\\np)\\(s:ng\\np),"
                    "\n       t(((s:ng\\np)\\(s:ng\\np))/((s:ng\\np)\\(s:ng\\np)), 'even', 'even', 'RB', 'I-ADVP', 'O'),"
                    "\n       t((s:ng\\np)\\(s:ng\\np), 'faster', 'faster', 'RBR', 'I-ADVP', 'O'))),"
                    "\n     fa((s:ng\\np)\\(s:ng\\np),"
                    "\n      t(((s:ng\\np)\\(s:ng\\np))/n, 'this', 'this', 'DT', 'I-NP', 'O'),"
                    "\n      ba(n,"
                    "\n       t(n, 'time', 'time', 'NN', 'I-NP', 'O'),"
                    "\n       t(n\\n, 'around', 'around', 'RB', 'I-ADVP', 'O')))))),"
                    "\n  t(period, '.', '.', '.', 'O', 'O'))).")
        assert parsed == expected

    def test_get_matching_tokens(self):
        in_list = ['Bob was arrested by the police']
        out_token_list = [
            {'bob': 'Bob', 'by': 'by', 'be': 'was', 'arrest': 'arrested', 'the': 'the', 'police': 'police'}]
        out_idx_list = [
            {'bob': (0, 0), 'by': (0, 3), 'be': (0, 1), 'arrest': (0, 2), 'the': (0, 4), 'police': (0, 5)}]
        out_pos_list = [
            {'bob': (0, 2), 'by': (17, 18), 'be': (4, 6), 'arrest': (8, 15), 'the': (20, 22), 'police': (24, 29)}]
        boxer = BoxerLocalAPI()

        for in_test, test in zip(in_list, out_token_list):
            output = boxer.get_matching_tokens(in_test, output="token")
            assert_equals(output, test)

        for in_test, test_idx in zip(in_list, out_idx_list):
            output = boxer.get_matching_tokens(in_test, output="token_pos")
            assert_equals(output, test_idx)

        for in_test, test_idx in zip(in_list, out_pos_list):
            output = boxer.get_matching_tokens(in_test, output="pos")
            assert_equals(output, test_idx)


# Web API
class TestBoxerWebAPI(TestCase):
    def test_web_parse_sentence_to_fol(self):
        sentence = 'John is running even faster this time around.'
        boxer = BoxerWebAPI()
        fol = boxer.sentence2FOL(sentence)
        assert str(fol) == ('[some(A,some(B,some(C,and(and(n1time(B),and(a1around(A),and('
                            'r1experiencer(A,B),pernamjohn(C)))),some(D,and(r1in(D,B),and(r1even(D),'
                            'and(r1faster(D),and(r1agent(D,C),v1run(D)))))))))).]')


# Abstract (Both)
class TestBoxerAbstract(TestCase):
    def test_expand_predicates(self):
        boxer = BoxerWebAPI()
        fol = FOL('')
        fol.info = ['some', ['A'],
                    ['some', ['B'],
                     ['some', ['C'],
                      ['and', ['and',
                               ['n1time', ['B']],
                               ['and',
                                ['a1around', ['A']],
                                ['and',
                                 ['r1Theme', ['A'], ['B']], ['pernamjohn', ['C']]]]],
                       ['some', ['D'],
                        ['some', ['E'],
                         ['and',
                          ['r1Time', ['E'], ['B']],
                          ['and',
                           ['r1even', ['E']],
                           ['and',
                            ['a1faster', ['D']],
                            ['and',
                             ['r1Manner', ['E'], ['D']],
                             ['and', ['r1Actor', ['E'], ['C']], ['v1run', ['E']]]]]]]]]]]]]
        expanded_fol = FOL('')
        expanded_fol.info = ['some', ['A'],
                             ['some', ['B'],
                              ['some', ['C'],
                               ['and', ['and',
                                        ['noun', ['B'], ['time']],
                                        ['and',
                                         ['adjective', ['A'], ['around']],
                                         ['and',
                                          ['theme', ['A'], ['B']],
                                          ['and', ['person', ['C']], ['noun', ['C'], ['john']]]]]],
                                ['some', ['D'],
                                 ['some', ['E'],
                                  ['and',
                                   ['relation', ['E'], ['B'], ['time']],
                                   ['and',
                                    ['relation', ['E'], ['even']],
                                    ['and',
                                     ['adjective', ['D'], ['faster']],
                                     ['and',
                                      ['relation', ['E'], ['D'], ['manner']],
                                      ['and', ['actor', ['E'], ['C']], ['verb', ['E'], ['run']]]]]]]]]]]]]
        boxer._expandFOLpredicates(fol)
        assert fol == expanded_fol

    def test_in_sequence_candc(self):
        boxer = BoxerLocalAPI()
        lf1 = boxer.sentence2LF('Get back to your station soldier! .')
        lf2 = boxer.sentence2LF('The trees grow to 25 feet.')

        assert str(
            lf1) == '[noun(c1,soldierc33),relation(c1,c0,of),noun(c0,station),relation(c1,c2,of),noun(c2,person),relation(c4,c1,to),adjective(c3,back),relation(c4,c3,manner),actor(c4,c5),verb(c4,get),noun(c5,thing)., topic(c0),noun(c0,c46).]'
        assert str(lf2) == '[noun(c0,tree),relation(c2,c1,to),noun(c1,foot),number(c1,25),actor(c2,c0),verb(c2,grow).]'

    # def test_expand_predicates(self):
    #    pass
