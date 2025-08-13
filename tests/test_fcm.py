import unittest
import numpy as np
import networkx as nx
from lib.fcm import FCM

class TestFCM(unittest.TestCase):

    def setUp(self):
        """
        Set up a test FCM based on the clot example.
        """
        self.fcm = FCM(title="Clot FCM Test")

        clot_edges = [
            (1, 1, 1), (1, 2, 0.4), (1, 3, 1), (1, 4, 1),
            (2, 3, 0.5), (2, 6, 0.45), (3, 2, 0.4), (3, 4, 0.75),
            (3, 6, 0.4), (4, 6, 0.4), (5, 6, 0.45), (6, 2, 0.7),
            (7, 5, -0.6), (8, 6, 0.95), (9, 10, -0.9), (10, 6, 1),
            (11, 8, 0.95), (12, 11, -0.6)
        ]
        # Use the correct networkx method name
        self.fcm.add_weighted_edges_from(clot_edges)

        mapping = {
            1: "HCP", 2: "stas", 3: "inju", 4: "HCF", 5: "ADP", 6: "PAgg",
            7: "clop", 8: "A2", 9: "war", 10: "K", 11: "cox", 12: "aspi"
        }
        # Relabel nodes using the networkx function directly
        self.fcm = nx.relabel_nodes(self.fcm, mapping, copy=False)

    def test_fcm_creation(self):
        """
        Test that the FCM is created correctly.
        """
        self.assertEqual(len(self.fcm.nodes()), 12)
        self.assertEqual(len(self.fcm.edges()), 18)
        self.assertEqual(self.fcm.get_edge_data("stas", "inju")[0]['weight'], 0.5)

    def test_evolve_once(self):
        """
        Test a single evolution step with a known input and output.
        """
        initial_vector_map = {
            "HCP": 1, "stas": 0, "inju": 1, "HCF": 0, "ADP": 0, "PAgg": 0,
            "clop": 0, "A2": 1, "war": 1, "K": 1, "cox": 0, "aspi": 0
        }

        node_order = list(self.fcm.nodes())
        initial_vector = np.array([initial_vector_map.get(node, 0) for node in node_order])
        mask = np.zeros_like(initial_vector)

        result_vector = self.fcm.evolve_once(initial_vector, mask)

        self.assertEqual(result_vector.shape, (12,))

    def test_analyze(self):
        """
        Test the analysis method.
        """
        analysis_results = self.fcm.analyze()
        self.assertIsInstance(analysis_results, dict)
        self.assertIn("in_degree", analysis_results)
        self.assertIn("out_degree", analysis_results)
        self.assertIn("betweenness_centrality", analysis_results)

        # Check a known value: PAgg has 5 incoming edges with a total weight of 3.65
        # The in_degree is a DegreeView object, which acts like a dict
        self.assertAlmostEqual(analysis_results["in_degree"]["PAgg"], 3.65)


if __name__ == '__main__':
    unittest.main()
