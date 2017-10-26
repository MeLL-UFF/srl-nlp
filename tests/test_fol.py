'''
Test unit for the fol.py module
'''
from fol import FOL

#UTILS

def list_eq(x,y):
    '''Compares two lists and checking if all their elements are equivalent'''
    if type(x) != list and type(x) != tuple:
        if type(x) == type(y):
            if x == y:
                return True
        return False
    if len(x) != len(y):
        return False
    for i in range(len(x)):
        if not list_eq(x[i],y[i]):
            return False
    return True

#TESTS

def test_split_someA_with_a_and():
    '''some(A, and(b, a(c)))'''
    f1 = FOL._split('some(A, and(b, a(c)))')
    l1 = ['some','(','A',',','and','(','b',',','a','(', 'c',')',')',')']
    assert list_eq(f1,l1)

def test_someA_with_a_and():
    '''some(A, and(b, a(c)))'''
    f1 = FOL('some(A, and(b, a(c)))').info
    l1 = ['some', ['A'], ['and', ['b'], ['a', ['c']]]]
    assert list_eq(f1,l1)

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
    assert list_eq(f1,l1)

def test_prenexform_someA_and_allB():
    '''some(A,and(4,all(B,a(4)))) -> some(A,all(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]]
    f.convert2PrenexForm()
    f1 = f.info
    l1 = ['some', ['A'], ['all', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert list_eq(f1, l1)

def test_skolemization_someA_and_allB():
    '''some(A,and(4,all(B,a(4)))) -> all(B,and(4,a(4)))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['all', ['B'], ['a', ['c']]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['all', ['B'], ['and', ['b'], ['a', ['c']]]]
    assert list_eq(f1, l1)

#TODO more skolemization tests

#TODO more prenexform test
sdef test_prenexform_someA_and_someB():
    '''some(A,and(4,all(B,a(4)))) -> some(A,some(B,and(4,a(4))))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.convert2PrenexForm()
    f1 = f.info
    l1 = ['some', ['A'], ['some', ['B'], ['and', ['b'], ['a', ['c']]]]]
    assert list_eq(f1, l1)

def test_skolemization_someA_and_someB():
    '''some(A,and(4,all(B,a(4)))) -> and(4,a(4))'''
    f  = FOL('')
    f.info = ['some', ['A'], ['and', ['b'], ['some', ['B'], ['a', ['c']]]]]
    f.skolemize()
    f1 = f.info
    l1 = ['and', ['b'], ['a', ['c']]]
    assert list_eq(f1, l1)

#TODO test negation