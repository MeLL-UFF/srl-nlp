"""
This file holds classes that facilitate rule manipulation
"""
from os import listdir, path, makedirs

import logging

import numpy as np
from tempfile import NamedTemporaryFile
from typing import List

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
    frontier = [lf.info]  # type: List
    while len(frontier):
        curr = frontier.pop()
        pred = curr[0]
        if pred == old_term:
            curr[0] = new_term
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

# FIXME
def remove_eq(lf, eq_term):
    """
    Remove the equality predicates and traverses the lf to bind all the constants that should be equal.
    The eq_term predicate must be binary, and the second term of it will be replaced by the first one in every
    predicate in the lf. When there are multiple eq_term predicates the final result might not be easy to predict but
    it is going to be correct.

    Args:
        lf: LF to have its eq predicates removed and the constants matched
        eq_term: equality predicate

    Returns:
        Nothing. This method changes the lf in-place
    """
    frontier = [lf.info]  # type: List
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


def get_chunks(elems, num_chunks):
    size = len(elems)
    step = int(np.floor(size / num_chunks))
    extra = size % num_chunks
    # Evenly distribute the number of elements in the chunks and keep ordering
    chunk_lens = [step + 1 if i < extra else step for i in range(num_chunks)]
    elems_iterator = elems.__iter__()
    for chunk_len in chunk_lens:
        yield [elems_iterator.next() for _ in range(chunk_len)]


def ensure_dir(dir_name):
    """
    Makes dir if there is no dir

        Returns if dir existed before this command
    """
    if not path.exists(dir_name):
        makedirs(dir_name)
        return False
    return True
