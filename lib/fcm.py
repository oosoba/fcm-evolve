import numpy as np
import networkx as nx
import matplotlib.pyplot as plt


class FCM(nx.MultiDiGraph):
    """
    A class for creating, manipulating, and visualizing Fuzzy Cognitive Maps.
    Inherits from networkx.MultiDiGraph, providing all its graph-related methods.
    """
    def __init__(self, title="FCM Example", **kwargs):
        """
        Initializes the FCM.

        Args:
            title (str): The title of the FCM, used for plotting.
            **kwargs: Additional keyword arguments passed to the networkx.MultiDiGraph constructor.
        """
        super().__init__(**kwargs)
        self.title = title
        self.activation_functions = {
            'step': lambda x: 1 * (x >= 0.5),
            'sigmoid': lambda x: 1 / (1 + np.exp(-x)),
            'tanh': lambda x: np.tanh(x)
        }
        self.activation_function = self.activation_functions['step']

    def set_activation(self, name_or_func):
        """
        Sets a custom activation function.

        Args:
            name_or_func (str or callable): The name of a predefined activation
                function ('step', 'sigmoid', 'tanh') or a callable function.
        """
        if isinstance(name_or_func, str):
            self.activation_function = self.activation_functions[name_or_func]
        else:
            self.activation_function = name_or_func

    def get_adjacency_matrix(self, sparse=True):
        """
        Returns the FCM's adjacency matrix.

        Args:
            sparse (bool): If True, returns a scipy sparse matrix. If False,
                returns a dense numpy matrix. Defaults to True.

        Returns:
            scipy.sparse_matrix or numpy.matrix: The adjacency matrix.
        """
        return nx.adjacency_matrix(self) if sparse else nx.adjacency_matrix(self).todense()

    def draw(self, **kwargs):
        """
        Visualizes the FCM graph structure using matplotlib.

        Args:
            **kwargs: Additional keyword arguments passed to networkx.draw.
        """
        pos = kwargs.pop('pos', nx.spring_layout(self))
        nx.draw(self, pos=pos, with_labels=True, **kwargs)
        plt.title(self.title)

    def _clamp(self, activation_vector, mask):
        """
        Clamps the activation vector based on a mask. Internal use.
        """
        clamped = activation_vector.copy()
        for i, val in enumerate(mask):
            if val == 1:
                clamped[i] = 1
            elif val < 0:
                clamped[i] = 0
        return clamped

    def evolve_once(self, activation_vector, mask):
        """
        Performs a single evolution step of the FCM.

        Args:
            activation_vector (np.array): The current activation state of the nodes.
            mask (np.array): A vector to clamp node values. 1 means always on,
                -1 means always off, 0 means normal evolution.

        Returns:
            np.array: The new activation state of the nodes after one step.
        """
        assert len(activation_vector) == len(mask)

        clamped_input = self._clamp(activation_vector, mask)
        adjacency_matrix = self.get_adjacency_matrix(sparse=True)

        raw_new_vector = adjacency_matrix.T.dot(clamped_input)
        activated_vector = self.activation_function(raw_new_vector)

        return self._clamp(activated_vector, mask)

    def evolve_to_limit(self, initial_vector, mask, nmax=1000):
        """
        Evolves the FCM until it reaches a fixed point or a limit cycle.

        Args:
            initial_vector (np.array): The starting activation state.
            mask (np.array): A vector to clamp node values.
            nmax (int): The maximum number of iterations to prevent infinite loops.

        Returns:
            numpy.matrix: A matrix where each row represents the activation
                state at each time step.
        """
        assert len(initial_vector) == len(mask)

        history = [initial_vector]
        seen_states = {tuple(initial_vector)}

        current_vector = initial_vector
        for _ in range(nmax):
            next_vector = self.evolve_once(current_vector, mask)

            next_vector_tuple = tuple(next_vector)
            if next_vector_tuple in seen_states:
                history.append(next_vector)
                break

            history.append(next_vector)
            seen_states.add(next_vector_tuple)
            current_vector = next_vector

        return np.matrix(history)

    def analyze(self):
        """
        Computes several network centrality metrics for the FCM.

        Returns:
            dict: A dictionary containing the in-degree, out-degree,
                betweenness, and eigenvector centrality for each node.
        """
        analysis = {
            "in_degree": self.in_degree(weight='weight'),
            "out_degree": self.out_degree(weight='weight'),
            "betweenness_centrality": nx.betweenness_centrality(self, weight='weight'),
            # Eigenvector centrality is not implemented for MultiDiGraph in networkx
            # "eigenvector_centrality": nx.eigenvector_centrality(self, weight='weight')
        }
        return analysis
