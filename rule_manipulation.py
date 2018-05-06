"""
This file holds classes that facilitate rule manipulation
"""
import logging
from copy import deepcopy as copy

import spacy
from regex import compile
from srl_nlp.fol import FOL
from srl_nlp.framenet.framenet import Description
from srl_nlp.logicalform import LF

logger = logging.getLogger(__name__)


############################

#          Utils           #

############################

def replace_all(lf, old_term, new_term):
    """

    Args:
        lf: Lf to be modified in-place
        old_term: the term to be replaced in all predicates
        new_term: the term to substitute the old_term in all the predicates of the lf

    Returns:
        Nothing. This method changes the lf in-place
    """
    frontier = [lf.info]
    while len(frontier):
        curr = frontier.pop()
        pred = curr[0]
        if pred == old_term:
            curr[0] = new_term
        frontier.extend(curr[1:])


def remove_eq(lf, eq_term='eq'):
    """
    Remove the equality predicates and traverses the lf to bind all the constants that should be equal.
    The eq_term predicate must be binary, and the second term of it will be replaced by the first one in every predicate in the lf.
    When there are multiple eq_term predicates the final result might not be easy to predict but it is going to be correct.

    Args:
        lf: LF to have its eq predicates removed and the constants matched
        eq_term: equality predicate

    Returns:
        Nothing. This method changes the lf in-place
    """
    frontier = [lf.info]
    while len(frontier):
        curr = frontier.pop()
        terms = curr[1:]
        pred = curr[0]
        if pred == eq_term:
            old_term = terms[1][0]
            new_term = terms[0][0]
            replace_all(lf, old_term, new_term)
        frontier.extend(curr[1:])
    frontier = [lf.info]
    while len(frontier):
        curr = frontier.pop()
        curr[:] = [curr[0]] + [child for child in curr[1:] if child[0] != eq_term]
        frontier.extend(curr[1:])


############################

#   Get Deep Role Rules    #

############################


def get_examples(target, fn):
    """
        Iterates over the fn frames that contain target as a Frame element and return every description element with the tag EXample

        Returns: list of (example, Frame) pairs
    """
    frames = fn.getFrameElementFrames(target)
    examples = []
    for frame in frames:
        fes = filter(lambda x: x == target, frame.coreFEs + frame.peripheralFEs)
        for fe in fes:
            examples.append((fe.definition.get_elements('ex'), frame))
    return examples


def get_preds(lf, token, skip=('relation',)):
    """
    Returns: a list of predicates that contain the token
    """
    out = []
    if not lf.get_pred() in skip:
        for term in lf.iterterms():
            if term.get_pred() == token:
                out.append(lf)
            else:
                out.extend(get_preds(term, token))
    return out


def _additive_dict_update(d1, d2):
    for key in d2:
        val = d1.get(key, [])
        val.extend(d2[key])
        d1[key] = val


# TODO get_tokens_index
def get_tokens_index(lf, tokenized, skip=('relation',)):
    """
    Returns:
        A dictionary of the form {term : [(i, token),...]},
        where term is a term of the lf and (i, tokens) are tuples
        with the index and tokens related to the predicate

    Parameters:
        lf:
        tokenized: list of strings
        skip: list of strings with the predicates to ignore
    """
    out = {}
    for i, token in enumerate(tokenized):
        if not lf.get_pred() in skip:
            for term in lf.iterterms():
                if term.get_pred() == token:
                    _additive_dict_update(out, {term: [(i, token)]})
                else:
                    _additive_dict_update(out, get_tokens_index(lf, tokenized))
    return out


def get_abbrev(frame):
    """
    Args:
        frame:

    Returns:
         A dictionary mapping abbreviation to Frame Element name

    """
    out = dict()
    for fe in frame.coreFEs + frame.peripheralFEs:
        if len(fe.abbrev) > 0:
            out[fe.abbrev] = fe.name
    return out


def get_annotations(example, lf, abbrev2fe=None, get_lemma=None):
    """
        This function matches the example annotations against the given lf
    (the lf must represent the example for this to make any sense)

    Args:
        example:
        lf:
        abbrev2fe: dict
        get_lemma:

    Returns:
        A tuple (fes_dict, taget_list), where
        fes_dict is a dictionary mapping Frame Element names to a predicate list
        target_list is a list of predicates that are target in this example
    """
    # TODO
    if abbrev2fe is None:
        abbrev2fe = dict()
    fes = dict()
    target = []
    if get_lemma is None:
        nlp = spacy.load('en_core_web_sm')
        get_lemma = lambda token: nlp(token.decode('utf-8'))[0].lemma_

    for term in example.content:
        pred_stack = []
        logger.debug("Example %s" % example)
        while isinstance(term, Description.FEeXample) or \
                isinstance(term, Description.Target) or \
                isinstance(term, Description.T):
            pred_stack.append(term.attribs.get('name', term.name))
            term = term.content[0]
        logger.debug("TERM stack %s" % pred_stack)
        if len(pred_stack) > 0:
            for token in term.strip().split(' '):
                try:
                    if len(token) < 1:
                        continue
                    literals = get_preds(lf, get_lemma(token))
                except IndexError as e:
                    logger.debug("Term: '%s'" % term)
                    raise e
                for literal in literals:
                    for pred in pred_stack:
                        pred = abbrev2fe.get(pred, pred)
                        if pred == 'target' or pred == 't':
                            target.append(literal)
                        elif pred == 'fex':
                            logger.error('Fex without attrib name')
                        else:
                            l = fes.get(pred, [])
                            l.append(literal)
                            fes[pred] = l
    return fes, target


def get_factors(lf, out=None):
    """
    Returns a mapping from the terms to predicate lists
    """
    if out == None:
        out = {}
    if FOL.is_operator(lf.get_pred()):
        for term in lf.iterterms():
            get_factors(term, out)
    else:
        for term in lf.iterterms():
            preds = out.get(term.get_pred(), [])
            preds.append(lf)
            out[term.get_pred()] = preds
    return out


def get_paths(predL, predR, factors, breadth=True):
    """
    Given two predicates in LF, and a mapping of their literals given
    by 'get_factors' this function yields the paths that can link
    those predicates.
    """
    frontier = [(predR, [])]
    visited = []
    while len(frontier):
        if breadth:
            curr_pred, path = frontier.pop(0)
        else:
            curr_pred, path = frontier.pop()
        if predL == curr_pred:
            yield path
        visited.append(curr_pred)
        for literal in curr_pred.iterterms():
            for term in set(factors.get(literal.get_pred(), [])):
                if not term in visited:
                    frontier.append((term, path + [term]))


def make_pred(literal, pred, *terms):
    """
    Returns a new LF predicate from the original pred, the literal and a label
    """
    t = pred.iterterms().next()
    terms = map(lambda x: x if isinstance(x, list) else [x], terms)
    out = LF()
    out.info = [literal] + [t.info] + terms
    return out


def str_preds(preds, pattern=compile('^c(\d+)$'), x=('frame_element', 'frame_related',
                                                     'relation'), count=None):
    """
    Converts a LF or a list of LFs into a string in a convenient way to be rendered in a rule
        LF -> str
        [LF] -> str
    """  # TODO improve description
    if not count:
        count = [0]

    def repl_const(match, count=count):
        count[0] = max(count[0], int(match.group(1)))
        return 'C' + match.group(1)

    def new_const(count=count):
        count[0] = count[0] + 1
        return 'C%d' % count[0]

    generalize = []
    if isinstance(preds, LF):
        pred = copy(preds)
        for literal in pred.iterterms():
            if literal.isleaf():
                logger.debug("PRED::%s: update:%s" % (pred.get_pred(), not pred.get_pred() in x))
                if pattern.match(literal.get_pred()):
                    literal.set_pred(pattern.sub(repl_const, literal.get_pred()))
                elif not pred.get_pred() in x:
                    generalize.append(literal)
        for literal in generalize:
            literal.set_pred(new_const())
        return pred.__repr__(final_dot=False)
    else:
        return ','.join(map(lambda p: str_preds(p, pattern, x, count), preds))
