'''
Test unit for the fol.py module
'''
from analysers.boxer import BoxerLocalAPI, BoxerWebAPI

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

##Local API

def test_parse_sentence_to_fol():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerLocalAPI()
    fol = boxer.sentence2FOL(sentence)
    assert fol == None, fol


def test_parse_sentence():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerLocalAPI()
    parsed = boxer._parse_sentence(sentence)
    assert parsed == None, parsed


##Web API

def test_parse_sentence_to_fol():
    sentence = 'John is running even faster this time around.'
    boxer = BoxerWebAPI()
    fol = boxer.sentence2FOL(sentence)
    assert fol == None, fol

#Abstract (Both)