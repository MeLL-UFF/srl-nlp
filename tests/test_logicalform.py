'''
Test unit for the logicalform.py module
'''
from srl_nlp.fol import FOL
from srl_nlp.logicalform import LF

def test_mary_does_not_like_cookies():
    fol = FOL()
    fol.info = ['some', ['A'], ['and', ['pernammary', ['A']], ['not', ['some', ['B'], ['some', ['C'], ['and', ['r1Theme', ['B'], ['C']], ['and', ['r1Actor', ['B'], ['A']], ['and', ['v1like', ['B']], ['n1cookie', ['C']]]]]]]]]]
    lf = LF(fol)
    lf_pred = ['all', ['B'], ['all', ['C'], ['and', ['pernammary', ['c0']], ['or', ['not', ['r1Theme', ['B'], ['C']]], ['not', ['r1Actor', ['B'], ['c0']]], ['not', ['v1like', ['B']]], ['not', ['n1cookie', ['C']]]]]]] 
    assert FOL._eq_predicate(lf.info, lf_pred)