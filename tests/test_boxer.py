'''
Test unit for the fol.py module
'''
from analysers.boxer import BoxerLocalAPI, BoxerWebAPI
from fol             import FOL

#TESTS

##Local API

def test_parse_sentence_to_fol():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerLocalAPI()
    fol = boxer.sentence2FOL(sentence)
    assert str(fol) == '[some(A,some(B,some(C,and(and(n1time(B),and(a1around(A),and(r1Theme(A,B),'+ \
                       'pernamjohn(C)))),some(D,some(E,and(r1Time(E,B),and(r1even(E),and(a1faster(D),' +\
                       'and(r1Manner(E,D),and(r1Actor(E,C),v1run(E)))))))))))).]'


def test_parse_sentence():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerLocalAPI()
    parsed = boxer._parse_sentence(sentence)
    parsed_lines = parsed.split('\n')[2:]
    parsed = '\n'.join(parsed_lines)
    assert parsed == ("\n:- op(601, xfx, (/))."
                      "\n:- op(601, xfx, (\))."
                      "\n:- multifile ccg/2, id/2."
                      "\n:- discontiguous ccg/2, id/2.\n"
                      "\nccg(1,"
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
                      "\n  t(period, '.', '.', '.', 'O', 'O'))).\n\n")


##Web API

def test_web_parse_sentence_to_fol():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerWebAPI()
    fol = boxer.sentence2FOL(sentence)
    assert str(fol) == '[some(A,some(B,some(C,and(and(n1time(B),and(a1around(A),and('+ \
                       'r1experiencer(A,B),pernamjohn(C)))),some(D,and(r1in(D,B),and(r1even(D),'+ \
                       'and(r1faster(D),and(r1agent(D,C),v1run(D)))))))))).]'

#Abstract (Both)

def test_expand_predicates():
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
                        ['some',['D'],
                         ['some',['E'],
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
                        ['noun', ['B'],['time']],
                         ['and',
                          ['adjective', ['A'], ['around']],
                          ['and',
                            ['theme', ['A'], ['B']], ['and', ['person', ['C']], ['noun', ['C'], ['john']]]]]],
                        ['some',['D'],
                         ['some',['E'],
                          ['and',
                           ['relation', ['E'], ['B'], ['time']],
                           ['and',
                            ['relation', ['E'], ['even']],
                            ['and',
                             ['adjective', ['D'], ['faster']],
                             ['and',
                              ['relation', ['E'], ['D'], ['manner']],
                              ['and', ['actor', ['E'], ['C']], ['verb', ['E'],['run']]]]]]]]]]]]]
    boxer._expandFOLpredicates(fol)
    assert fol == expanded_fol

#test_expand_predicates()