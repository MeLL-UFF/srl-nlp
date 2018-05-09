from logicalform import LF
from rule_utils import _additive_dict_update, make_pred


# parser = NetXMLParser()
# fn = parser.parse('framenet/fndata-1.7')


def test_replace_all():
    assert False


def test_remove_eq():
    assert False


def test_additive_dict_update():
    d1 = {'a': [0], 'c': [2], 'b': [1], 'e': [4], 'd': [3], 'f': [5]}
    d2 = {'e': [1], 'd': [0], 'g': [3], 'f': [2], 'i': [5], 'h': [4], 'k': [7], 'j': [6]}

    _additive_dict_update(d1, d2)
    d_correct = {'a': [0],
                 'b': [1],
                 'c': [2],
                 'd': [3, 0],
                 'e': [4, 1],
                 'f': [5, 2],
                 'g': [3],
                 'h': [4],
                 'i': [5],
                 'j': [6],
                 'k': [7]}
    assert d1 == d_correct


def test_get_examples():
    assert False


def test_get_preds():
    assert False


def test_get_tokens_index():
    assert False


def test_get_abbrev():
    assert False


def test_get_annotations():
    assert False


def test_get_factors():
    assert False


def test_get_paths():
    assert False


def test_make_pred():
    literal = 'magic'
    pred = LF()
    pred.info = ['Nonsense', ['dumbledore'], ['B']]
    extra = ['hpotter', 'rweasley', 'hgranger']
    correct = "magic(dumbledore,hpotter,rweasley,hgranger)."

    test = str(make_pred(literal, pred, *extra))

    print(test)
    print(correct)
    assert test == correct


def test_str_preds():
    assert False
