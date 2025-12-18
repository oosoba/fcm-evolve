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

    def evolve_asynchronous(self, activation_vector, mask, subset_size=1):
        """
        Performs a single asynchronous evolution step on a subset of nodes.

        Args:
            activation_vector (np.array): The current activation state.
            mask (np.array): A vector to clamp node values.
            subset_size (int): The number of nodes to update asynchronously.

        Returns:
            np.array: The new activation state.
        """
        new_vector = activation_vector.copy()

        # Get indices of nodes that are not masked off
        unmasked_indices = np.where(mask == 0)[0]

        if len(unmasked_indices) == 0:
            return new_vector

        # Choose a random subset of unmasked nodes to update
        update_indices = np.random.choice(
            unmasked_indices,
            size=min(subset_size, len(unmasked_indices)),
            replace=False
        )

        # Calculate the full potential next state
        full_next_vector = self.evolve_once(activation_vector, mask)

        # Only update the selected nodes
        for i in update_indices:
            new_vector[i] = full_next_vector[i]

        return new_vector

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

    @staticmethod
    def join(fcms, weights=None, title="Joined FCM"):
        """
        Combines a list of FCMs using weighted addition.

        Args:
            fcms (list): A list of FCM objects to join.
            weights (list, optional): A list of weights for each FCM.
                If None, FCMs are weighted equally. Defaults to None.
            title (str, optional): The title for the new joined FCM.

        Returns:
            FCM: A new FCM object representing the joined map.
        """
        if weights is None:
            weights = [1 / len(fcms)] * len(fcms)
        else:
            weights = np.array(weights) / np.sum(weights)

        all_nodes = set()
        for fcm in fcms:
            all_nodes.update(fcm.nodes())

        new_fcm = FCM(title=title)
        new_fcm.add_nodes_from(sorted(list(all_nodes)))

        edge_weights = {}
        for fcm, fcm_weight in zip(fcms, weights):
            for u, v, data in fcm.edges(data=True):
                weight = data.get('weight', 0)
                if (u, v) not in edge_weights:
                    edge_weights[(u, v)] = 0
                edge_weights[(u, v)] += fcm_weight * weight

        for (u, v), weight in edge_weights.items():
            if weight != 0:
                new_fcm.add_edge(u, v, weight=weight)

        return new_fcm

    @staticmethod
    def join_by_vote(fcms, weights=None, title="Voted Joined FCM"):
        """
        Combines a list of FCMs using majority vote before weighted addition.
        An edge is only included if it exists in at least half of the FCMs.

        Args:
            fcms (list): A list of FCM objects to join.
            weights (list, optional): A list of weights for each FCM.
            title (str, optional): The title for the new joined FCM.

        Returns:
            FCM: A new FCM object representing the joined map.
        """
        n = len(fcms)
        edge_votes = {}
        for fcm in fcms:
            for u, v in fcm.edges():
                if (u, v) not in edge_votes:
                    edge_votes[(u, v)] = 0
                edge_votes[(u, v)] += 1

        vetoed_edges = {edge for edge, count in edge_votes.items() if count < n / 2}

        # Create copies of FCMs and remove vetoed edges
        filtered_fcms = []
        for fcm in fcms:
            fcm_copy = fcm.copy()
            fcm_copy.remove_edges_from(vetoed_edges)
            filtered_fcms.append(fcm_copy)

        return FCM.join(filtered_fcms, weights, title)

    @staticmethod
    def create_initial_vector(fcm, active_nodes):
        """
        Creates an initial activation vector with specified nodes active.

        Args:
            fcm (FCM): The FCM object.
            active_nodes (list): A list of node names to activate.

        Returns:
            np.array: The initial activation vector.
        """
        node_order = list(fcm.nodes())
        initial_vector = np.zeros(len(node_order))
        for node in active_nodes:
            if node in node_order:
                idx = node_order.index(node)
                initial_vector[idx] = 1
        return initial_vector

    def visualize_evolution(self, history, output_dir="evolution_steps"):
        """
        Generates a series of images visualizing the FCM's evolution.

        Args:
            history (numpy.matrix): The output from evolve_to_limit.
            output_dir (str, optional): The directory to save the image files.
                Defaults to "evolution_steps".
        """
        import os
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        node_order = list(self.nodes())

        for i, state in enumerate(np.asarray(history)):
            plt.figure(figsize=(10, 10))

            # Create a color map based on activation levels
            colors = []
            for node in node_order:
                node_index = node_order.index(node)
                activation = state[node_index]
                # Green for active, red for inactive, with intensity
                colors.append((1 - activation, activation, 0))

            self.draw(node_color=colors)
            plt.title(f"{self.title} - Step {i}")

            filename = os.path.join(output_dir, f"step_{i:03d}.png")
            plt.savefig(filename)
            plt.close()

    def learn(self, data, rule='dhl', learning_rate=0.1):
        """
        Updates the FCM's edge weights based on time-series data using a Hebbian learning rule.

        Args:
            data (np.array): A 2D array where rows are nodes and columns are time steps.
            rule (str, optional): The learning rule to use ('dhl', 'hebbian', 'ghl').
                Defaults to 'dhl'.
            learning_rate (float, optional): The learning rate for the update rule.
                Defaults to 0.1.
        """
        if not self.nodes():
            raise ValueError("FCM must have nodes before learning.")

        node_order = list(self.nodes())
        if data.shape[0] != len(node_order):
            raise ValueError("Data shape must match the number of nodes.")

        # Create a mapping from node name to its index in the data array
        name_to_index = {name: i for i, name in enumerate(node_order)}

        delta_data = np.diff(data, axis=1)

        for u, v, d in self.edges(data=True):
            idx_u = name_to_index[u]
            idx_v = name_to_index[v]

            C0 = data[idx_u, :]
            C1 = data[idx_v, :]

            delta_C0 = np.concatenate(([0], delta_data[idx_u, :]))
            delta_C1 = np.concatenate(([0], delta_data[idx_v, :]))

            weight = d.get('weight', 0)

            for t in range(data.shape[1]):
                if rule == 'dhl':
                    update = learning_rate * (delta_C0[t] * delta_C1[t] - weight)
                elif rule == 'hebbian':
                    update = learning_rate * (C0[t] * C1[t] - weight)
                elif rule == 'ghl':
                    dhl_update = learning_rate * (delta_C0[t] * delta_C1[t] - weight)
                    hebbian_update = learning_rate * (C0[t] * C1[t] - weight)
                    update = dhl_update + hebbian_update
                else:
                    raise ValueError(f"Unknown learning rule: {rule}")

                weight += update

            d['weight'] = weight
