# 31-Dec-2016
import numpy as np
# import scipy as sp import os, re, json, csv
import networkx as nx
import matplotlib.pyplot as plt

# from pylab import rcParams
# rcParams['figure.figsize'] = 7,4 # Bigger figures
# rcParams['lines.linewidth'] = 2.0


class FCM:
    '''
    Fuzzy Cognitive Map (FCM) class. Version 0.2
    Does both FCM representation, manipulation, and visualization.

    Representation: FCM's are inherently directed graphs (digraphs).
    Using networkx graphs as base.
    [OO-NB: consider inheriting nx.Digraph directly]

    Manipulation: Can evolve FCM from input state to either one-step advance or
    to convergent fixed-point. Both subject to a node-clamping mask.
    [OO-NB: need to figure out how identify non-fixed-point limit cycles]

    Visualization: Graph plot of FCM structure. Just using default layout.

    Version 0.2 Changes:
    - switch from Digraph to MultiDigraph to allow for self-loops
    -

    To Do:
    - Evolve to limit-cycle sequence
    - Visualize evolutions on FCM graph
    - Backward Inference (!!!***)
    '''
    __version__ = "0.2"

    def __init__(self,
                 title="FCM Example"):  # edgeList, edgeLabels,
        self.title = title
        self.graph = nx.MultiDiGraph()
        self.ActivationFunction = lambda x: 1*(x >= 0.5)
        # S fxn; use member fxn to update
        return

    def add_edges(self, edge_lst):
        self.graph.add_weighted_edges_from(edge_lst)

    def label_edges(self, label_dict):
        self.graph = nx.relabel_nodes(self.graph, label_dict, copy=False)

    def set_activation(self, actvn):
        self.ActivationFunction = actvn
        return

    def get_FCM_Matrix(self):
        return(nx.adjacency_matrix(self.graph).todense())

    def VizFCM(self):
        nx.draw(self.graph,
                with_labels=True, node_size=700,
                nodecolor='g', edge_color='b')
        plt.title(self.title)
        return

    def EvolveOnce(self, inp, mask):
        assert(len(inp) == len(mask))
        return Clamp(self.ActivationFunction(
            np.asarray(
                np.matmul(
                    Clamp(inp, mask),
                    nx.adjacency_matrix(self.graph).todense())).ravel()
        ), mask)

    def EvolveToLimit(self, inp, mask, nmax=1000):
        '''EvolveToLimit(inp, mask, nmax) evolves FCM (self) to convergence
        using inp as a starting point. mask fixing specific inputs nodes'''
        assert(len(inp) == len(mask))
        seq = NestWhileList(
            lambda inp_vec: self.EvolveOnce(inp_vec, mask),
            inp, stopcritQ, nmax
        )
        seq = [inp] + seq
        return(np.matrix(seq))

    def VizFCMEvol(self):  # needs implementation...
        pass


def NestWhileList(func, arg, stopTestQ, nmax=1000):
    # stopTestQ takes full FP list to calc stop flag. stop when True
    tmp_lst = [func(arg)]
    tmp_lst = tmp_lst + [func(tmp_lst[-1])]
    while (not(stopTestQ(tmp_lst)) & (len(tmp_lst) < nmax)):
        tmp_lst = tmp_lst + [func(tmp_lst[-1])]
    return([list(t) for t in tmp_lst])


def stopcritQ(res_lst):
    res_lst = [str(t) for t in res_lst]
    return(len(set(res_lst)) != len(res_lst))


def Clamp(actvn, mask):
    assert(len(actvn) == len(mask))
    clamped = actvn
    clamped[[j for j in range(len(mask)) if mask[j] == 1]] = 1
    clamped[[j for j in range(len(mask)) if mask[j] < 0]] = 0
    return clamped


def dec2binList(x):
    return [int(k) for k in list(bin(x)[2:])]


def ClampIntInt(actvn, on, off):
    return Clamp(actvn, dec2binList(on), dec2binList(off))
