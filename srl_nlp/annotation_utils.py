from copy import deepcopy as copy
from re import compile

import spacy

from srl_nlp.data_augmentation.logical_representation.fol import FOL
from srl_nlp.data_augmentation.logical_representation.logicalform import LF

from srl_nlp.framenet import description
from srl_nlp.rule_utils import logger, _additive_dict_update


############################

#   Get Deep Role Rules    #

############################


def get_annotations(example, lf, abbrev2fe=None, get_lemma=None):
    """
    This function matches the example annotations against the given lf
    (the lf must represent the example for this to make any sense)

    Args:
        example:
        lf:  a lf representation of the example
        abbrev2fe: dict of abbreviations to frame element names
        get_lemma: function that returns the lemma of a given token

    Returns:
        A tuple (fes_dict, taget_list), where
        fes_dict is a dictionary mapping Frame Element names to a predicate list
        target_list is a list of predicates that are target in this example
    """
    # TODO check if it works okay when we have the same instance of predicates (conjunctions)
    if abbrev2fe is None:
        abbrev2fe = dict()
    fes = dict()  # type: Dict
    target = []
    if get_lemma is None:
        nlp = spacy.load('en_core_web_sm')

        def get_lemma(token_str):
            return nlp(token_str.decode('utf-8'))[0].lemma_.encode('utf-8')

    for term in example.content:
        pred_stack = []
        logger.debug("Example %s" % example)
        while isinstance(term, description.FEeXample) or \
                isinstance(term, description.Target) or \
                isinstance(term, description.T):
            if len(term.content) == 0:
                logger.warning("Term {} is empty in example {}".format(str(term), str(example)))
                break
            else:
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
    # type: (object, object, Dict[str,Set[LF]], bool) -> Iterable[List[LF]]
    """
    Given two predicates in LF, and a mapping of their literals given
    by 'get_factors' this function yields the paths that can link
    those predicates.
    """
    frontier = [(pred_r, [])]  # type: List
    visited = []
    while len(frontier):
        if breadth:
            curr_pred, search_path = frontier.pop(0)
        else:
            curr_pred, search_path = frontier.pop()
        if pred_l == curr_pred:
            yield search_path
        visited.append(curr_pred)
        for literal in curr_pred.iterterms():
            for term in set(factors.get(literal.get_pred(), [])):
                if term not in visited:
                    frontier.append((term, search_path + [term]))


def make_pred(literal, pred, *terms):
    """
    Returns a new LF predicate from the original pred, the literal and a label.
    Only the first term of pred is used in the final predicate
    """
    t = pred.iterterms().next()
    terms = map(lambda x: x if isinstance(x, list) else [x], terms)
    out = LF()
    out.info = [literal] + [t.info] + list(terms)
    return out


def str_preds(preds, pattern=compile(r'^c(\d+)$'), x=('frame_element', 'frame_related',
                                                      'relation'), count=None):
    """
    Converts a LF or a list of LFs into a string in a convenient way to be rendered in a rule
        LF -> str
        [LF] -> str
    """
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
