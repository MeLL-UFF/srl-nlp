'''
Test unit for the fol.py module
'''
from fol import FOL

#TESTS

def test_split_someA_with_a_and():
    '''some(A, and(b, a(c)))'''
    f1 = FOL._split('some(A, and(b, a(c)))')
    l1 = ['some','(','A',',','and','(','b',',','a','(', 'c',')',')',')']
    assert FOL._eq_predicate(f1,l1)

def test_someA_with_a_and():
    '''some(A, and(b, a(c)))'''
    f1 = FOL('some(A, and(b, a(c)))').info
    l1 = ['some', ['A'], ['and', ['b'], ['a', ['c']]]]
    assert FOL._eq_predicate(f1,l1)

# def test_repeated_comma_error():
#     '''some(A, and(b, a(c)))'''
#     try:
#         f1 = FOL('some(A, , and(b, a(c)))').info
#     except:
#         return
#     assert False #No exception raised

def test_someA_allBwith_a_and():
    '''some(A, all(B, and(b, a(c))))'''
    f1 = FOL('some(A, all(B, and(b, a(c))))').info
    l1 = ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1,l1)

def test_push_quantifiers_someA_and_someB():
    '''some(A,and(4,all(B,a(4)))) -> some(A,some(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.info = FOL._push_quantifiers(f.info)
    f1 = f.info
    l1 = ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1, l1)

def test_skolemization_someA_and_allB_someC():
    '''some(A,and(4,all(B,a(4)))) -> all(B,and(4,a(4)))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['some', ['C'], ['a', ['C']]]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['some', ['A'], ['all', ['B'], ['some', ['C'], ['and', ['b'], ['a', ['C']]]]]]
    assert FOL._eq_predicate(f1, l1)

def test_push_negation_no_change():
    '''some(A,and(4,all(B,a(4)))) -> some(A,some(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.info = FOL._push_negation(f.info)
    f1 = f.info
    l1 = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1, l1)

def test_push_negation_some():
    '''some(A,and(b,not(some(B,a(4))))) -> some(A,and(b,all(B,not(a(4))))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['not', ['some', ['B'], ['a', ['c']]]]]]
    f.info = FOL._push_negation(f.info)
    f1 = f.info
    l1 = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['not', ['a', ['c']]]]]]
    assert FOL._eq_predicate(f1, l1)


def test_push_negation_begining():
    '''some(A,and(b,not(some(B,a(4))))) -> some(A,and(b,all(B,not(a(4))))))'''
    f  = FOL('')
    f.info = ['not', ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]]
    f.info = FOL._push_negation(f.info)
    f1 = f.info
    l1 = ['all', ['A'], ['or', ['not', ['b']], ['all', ['B'], ['not', ['a', ['c']]]]]]
    assert FOL._eq_predicate(f1, l1)


def test_prenexform_someA_and_allB():
    '''some(A,and(4,all(B,a(4)))) -> some(A,all(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]]
    f.convert2PrenexForm()
    f1 = f.info
    l1 = ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1, l1)


#TODO more skolemization tests

def test_skolemization_someA_and_allB():
    '''some(A,and(4,all(B,a(4)))) -> all(B,and(4,a(4)))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['all', ['B'], ['and', ['b'], ['a', ['c']]]]
    assert FOL._eq_predicate(f1, l1)

def test_skolemization_someA_and_allB_someC():
    '''some(A,and(4,all(B,a(4)))) -> all(B,and(4,a(4)))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['some', ['C'], ['a', ['C']]]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['all', ['B'], ['and', ['b'], ['a', ['c1']]]]
    assert FOL._eq_predicate(f1, l1)

#TODO more prenexform test
def test_prenexform_someA_and_someB():
    '''some(A,and(4,all(B,a(4)))) -> some(A,some(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.convert2PrenexForm()
    f1 = f.info
    l1 = ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert FOL._eq_predicate(f1, l1)

def test_skolemization_someA_and_someB():
    '''some(A,and(4,all(B,a(4)))) -> and(4,a(4))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['and', ['b'], ['a', ['c']]]
    assert FOL._eq_predicate(f1, l1)

#TODO test negation