import math
from sys import stderr

import logging
import matplotlib.pyplot as plt
import networkx as nx
from copy import deepcopy as copy
from functools import partial
from networkx.drawing.nx_pydot import write_dot
from os import path

from fol import FOL

logger = logging.getLogger(__name__)


class LF:
    def __init__(self, fol=None, header=None, add_quotm=False, remove_for_alls=False, constant_prefix='c',
                 skolemize_ignore=None):
        """Creates a new Logical Formula
        LF(fol) -> lf

        The first parameter must be the source to be used to generate the LF.
        If it is a FOL, you can specify the argument 'header' to eliminate the header if it is there

        Args:
            constant_prefix: optional, must be called by name. Defines the prefix of the constants to
                             replace the variables in the FOL during the generation of the LF
            header: optional, must be called by name. First predicate of the FOL that should be ignored.
            add_quotm: bool. Adds quotation marks to all terms that are not constants
        """
        if fol is not None:
            fol = copy(fol)
            if isinstance(fol, str):
                fol = FOL(fol)
            if header and fol.info[0] == header:
                fol.info = fol.info[-1]
            #    print "*"
            # print ">", header, fol
            try:
                # print '\n-', fol
                logger.debug('FOL: %s', fol)
                fol.convert2PrenexForm()
                logger.debug('PRENEX FOL: %s', fol)
                # print '*', fol
                if skolemize_ignore is not None:
                    fol.skolemize(constant_prefix=constant_prefix, add_quotm=add_quotm, ignore=skolemize_ignore,
                                  removeForAlls=remove_for_alls)
                else:
                    fol.skolemize(constant_prefix=constant_prefix, add_quotm=add_quotm,
                                  removeForAlls=remove_for_alls)
                logger.debug('SKOLEMIZED FOL: %s', fol)
                fol.push_operand(FOL.OR)
                self.info = fol.info
                logger.debug('LF: %s', self)
            except AttributeError as e:
                raise Exception('Not a valid FOL%s\t Error: %s' % (fol, e))
        else:
            self.info = []

    def split(self):
        """Split the clause into many smaller clauses"""
        frontier = [self.info]  # type: list
        out = []  # type: list
        while len(frontier) > 0:
            root = frontier.pop()
            while FOL.is_quantifier(root[0]):
                assert len(root) > 1, 'Invalid FOL'
                root = root[-1]
            # print '##' if root[0] == FOL.AND else ('xx'+str(root[0]))
            if root[0] == FOL.AND:
                frontier.extend(root[1:])
            else:
                lf = LF()
                lf.info = copy(root)
                out.append(lf)
        return out

    def iterterms(self):
        """Returns un iterator that yields the terms of this LF as LFs 
        themselves
        """
        if len(self.info) > 0:
            for term in self.info[1:]:
                out = LF()
                out.info = term
                yield out

    def get_pred(self):
        """Returns the literal of the top predicate of this LF, usually
        it is going to be an 'AND'
        """
        if len(self.info) > 0:
            return self.info[0]
        return None

    def set_pred(self, pred):
        """Updates the literal of the top predicate of this LF
        """
        if len(self.info) > 0:
            self.info[0] = pred
        return None

    def has_pred(self, pred, avoid_leaf=True):
        if hasattr(pred, 'get_pred'):
            pred = pred.get_pred()
        if self.get_pred() == pred:
            return True
        else:
            for term in self.iterterms():
                if not avoid_leaf or not term.isleaf():
                    if term.has_pred(pred):
                        return True
        return False

    def isleaf(self):
        return len(self.info) == 1

    def __hash__(self):
        return repr(self).__hash__()

    def __eq__(self, other):
        if self is None or other is None:
            if self is None and other is None:
                return True
            else:
                return False
        return FOL.equals_predicate(self.info, other.info)

    @staticmethod
    def _repr_aux(term, and_t, or_t, supress_not):
        try:
            parser = partial(LF._repr_aux, and_t=and_t, or_t=or_t, supress_not=supress_not)
            if supress_not:
                term = filter(lambda x: x[0] != FOL.NOT, term)
            if term[0] == FOL.AND:
                out = '%s' % and_t.join(map(parser, term[1:]))
            elif term[0] == FOL.OR:
                out = '(%s)' % or_t.join(map(parser, term[1:]))
            # elif (term[0] == FOL.ALL or term[0] == FOL.EXISTS) and len(term) > 2:
            #    out = parser(term[-1])
            else:
                out = term[0]
                if len(term) > 1:
                    out += '(%s)' % ','.join(map(parser, term[1:]))
        except Exception as e:
            print >> stderr, e
            raise Exception('Ill-formed FOL:%s' % term)
        return out

    def __repr__(self, and_t=',', or_t=';', suppress_not=False, final_dot=True):
        """
        Return a str representation of the LF
        """
        if self.info is None or len(self.info) < 1:
            out = ''
        else:
            term = self.info
            dot = '.' if final_dot else ''
            out = LF._repr_aux(term, and_t, or_t, suppress_not) + dot
        return out

    def plot_lf(self, fig_name=None, on_screen=True):
        """
        Plots the given lf to the screen and file.

        Args:
            self: the lf to be plotted into a network graph
            fig_name: name of the output_file (I recommend the extension svg).
                      Optional. If left None, it is not gonna save the graph in any file.
            on_screen: boolean value. Try to plot on screen. Optional.

        Returns:
            networkx.graph and dict {edge:label} from the lf
        """
        # TODO choose the plot engine instead of infer it
        frontier = [self]  # type: list
        edges = []  # type: list
        labels = []  # type: list

        while len(frontier) > 0:
            term = frontier.pop()
            for child in term.iterterms():
                if child.isleaf():
                    edges.append(_fix_edge(term.info[1:]))
                    if term.info[0] == 'relation':
                        label = 'rel(%s)' % term.info[-1][0]
                    else:
                        label = term.info[0]
                    labels.append(label)
                    logger.debug("TERM -> %s" % term)
                    break
                else:
                    frontier.insert(0, child)

        g = nx.DiGraph()
        edge_labels = {edge: label for (edge, label) in zip(edges, labels)}
        for edge, label in zip(edges, labels):
            g.add_edge(edge[0], edge[1], label=label)
        pos = nx.fruchterman_reingold_layout(g, k=2 / math.sqrt(g.order()), scale=10)

        if on_screen:
            _plot_aux(edge_labels, g, pos)
            plt.show()
        if fig_name is not None:
            _, fig_ext = path.splitext(fig_name)
            if fig_ext.lower() == '.dot':
                write_dot(g, fig_name)
            else:
                _plot_aux(edge_labels, g, pos)
                plt.savefig(fig_name)
        plt.clf()
        return g, edge_labels


def _fix_edge(edge):
    edge = tuple(map(lambda x: x[0], edge))
    if len(edge) == 1:
        return edge + edge
    if len(edge) > 1:
        return edge[:2]
    return edge


def _plot_aux(edge_labels, g, pos):
    """
    Define all the parameters for a standard plot of an LF
    Args:
        edge_labels: dict of the form {edge:label}
        g: a networkx graph
        pos: position of nodes given by a networkx.layout method

    Returns:
        Nothing. The side effects of this methods consists of plotting the lf
        graph into the current matplot.pyplot figure.
    """
    nx.draw_networkx_edge_labels(g, pos, edge_labels=edge_labels, font_size=5)
    nx.draw_networkx(g, pos, node_size=100, font_size=5)
    plt.axis('off')
