"""
This file holds classes that facilitate rule manipulation
"""
from os import listdir

import logging
import spacy
from copy import deepcopy as copy
from regex import compile
from tempfile import NamedTemporaryFile

from srl_nlp.framenet.framenet import Description
from srl_nlp.logical_representation.fol import FOL
from srl_nlp.logical_representation.logicalform import LF

logger = logging.getLogger(__name__)


############################

#          Utils           #

############################

def not_none_to_str(obj):
    if obj is not None:
        return str(obj)
    return obj


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


# FIXME
def remove_eq(lf, eq_term):
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


def _additive_dict_update(d1, d2):
    """
        Extends all values in d1 that match keys with values in d2
    Args:
        d1: dictionary whose items are lists
        d2: dictionary whose items are iterables

    Returns:
        Nothing, the change is do inplace in d1

    """
    for key in d2:
        val = d1.get(key, [])
        val.extend(d2[key])
        d1[key] = val


############################

#   Get Deep Role Rules    #

############################


def get_examples(target, fn):
    """
        Iterates over the fn frames that contain target as a Frame element and
        returns every description element with the tag EXample

        Returns: list of (example, Frame) pairs
    """
    frames = fn.get_frame_element_frames(target)
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
    out = set()
    if not lf.get_pred() in skip:
        for term in lf.iterterms():
            if term.get_pred() == token:
                out.add(lf)
            else:
                out.update(get_preds(term, token))
    return out


def get_tokens_index(lf, tokenized, skip=('relation',)):
    """

    Parameters:
        lf:
        tokenized: list of strings
        skip: list of strings with the predicates to ignore

    Returns:
        A dictionary of the form {term : [(i, token),...]},
        where term is a term of the lf and (i, tokens) are tuples
        with the index and tokens related to the predicate
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


def get_abbrev(frame, lower_case=True):
    """
    Args:
        frame: A Frame from a FrameNet object
        lower_case: boolean value. If true, it converts the frame abbreviation to lower case

    Returns:
         A dictionary mapping abbreviation to Frame Element name

    """
    out = dict()
    for fe in frame.coreFEs + frame.peripheralFEs:
        if len(fe.abbrev) > 0:
            if lower_case:
                out[fe.abbrev.lower()] = fe.name
            else:
                out[fe.abbrev] = fe.name
    return out


def get_annotations(example, lf, abbrev2fe=None, get_lemma=None):
    """
    This function matches the example annotations against the given lf
    (the lf must represent the example for this to make any sense)

    Args:
        example:
        lf:  a lf representation of the example
        abbrev2fe: dict of abbreviations to frame element names
        get_lemma: function that retorns the lemma of given token

    Returns:
        A tuple (fes_dict, taget_list), where
        fes_dict is a dictionary mapping Frame Element names to a predicate list
        target_list is a list of predicates that are target in this example
    """
    # TODO check if it works okay when we have the same instance of predicates (conjunctions)
    if abbrev2fe is None:
        abbrev2fe = dict()
    fes = dict()
    target = []
    if get_lemma is None:
        nlp = spacy.load('en_core_web_sm')

        def get_lemma(token_str):
            return nlp(token_str.decode('utf-8'))[0].lemma_

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
                        pred = abbrev2fe.get(pred.lower(), pred)
                        if pred == 'target' or pred == 't':
                            target.append(literal)
                        elif pred == 'fex':
                            logger.error('Fex without attrib name')
                        else:
                            temp_list = fes.get(pred, [])
                            temp_list.append(literal)
                            fes[pred] = temp_list
    return fes, target


def get_factors(lf, out=None):
    """
    Returns a mapping from the terms to predicate lists
    """
    if out is None:
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


def get_paths(pred_l, pred_r, factors, breadth=True):
    """
    Given two predicates in LF, and a mapping of their literals given
    by 'get_factors' this function yields the paths that can link
    those predicates.
    """
    frontier = [(pred_r, [])]
    visited = []
    while len(frontier):
        if breadth:
            curr_pred, path = frontier.pop(0)
        else:
            curr_pred, path = frontier.pop()
        if pred_l == curr_pred:
            yield path
        visited.append(curr_pred)
        for literal in curr_pred.iterterms():
            for term in set(factors.get(literal.get_pred(), [])):
                if term not in visited:
                    frontier.append((term, path + [term]))


def make_pred(literal, pred, *terms):
    """
    Returns a new LF predicate from the original pred, the literal and a label.
    Only the first term of pred is used in the final predicate
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

    def repl_const(match, idx=count):
        idx[0] = max(idx[0], int(match.group(1)))
        return 'C' + match.group(1)

    def new_const(idx=count):
        idx[0] = idx[0] + 1
        return 'C%d' % idx[0]

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


def open_a_file(name=None, mode='wr'):
    """Opens the file, if no name is given, opens a NamedTemporaryFile"""
    if name is not None:
        return open(name, mode)
    else:
        return NamedTemporaryFile(mode=mode)


def list_doc_files(folder_path):
    """
    Returns list of basenames of the files that have the appropriate format to be a document.
    Args:
        folder_path: string with the folder path where the documents files are stored
    Returns:
        List of strings.
    """
    return [f for f in listdir(folder_path)
            if f.lower().endswith('.xml') or f.lower().endswith('.json')]
