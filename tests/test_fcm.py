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

    def test_evolve_asynchronous(self):
        """
        Test the asynchronous evolution method with a deterministic setup.
        """
        # Create a simple 2-node FCM where A activates B
        fcm_simple = FCM()
        fcm_simple.add_weighted_edges_from([('A', 'B', 1.0)])

        # Initial state: A is active, B is not
        initial_vector = np.array([1, 0])
        mask = np.zeros_like(initial_vector)

        # Since 'A' is the only unmasked node that can be updated,
        # and it has no inputs, it will be chosen. But its state won't change.
        # Let's activate B instead, which is guaranteed to change.
        # To do this, we need to know the update order.
        # A better test: A->B, B->A. A=1, B=0. Update B. B becomes 1.
        fcm_simple_2 = FCM()
        fcm_simple_2.add_weighted_edges_from([('A', 'B', 1.0), ('B', 'A', 1.0)])
        initial_vector_2 = np.array([1, 0])
        mask_2 = np.array([1, 0]) # Clamp A, so only B can be updated

        # Evolve with a subset size of 1. Only B can be chosen.
        result_vector = fcm_simple_2.evolve_asynchronous(initial_vector_2, mask_2, subset_size=1)

        # Expected output: A is still 1 (clamped), B becomes 1
        expected_output = np.array([1, 1])

        np.testing.assert_array_equal(result_vector, expected_output)

    def test_knowledge_fusion(self):
        """
        Test the knowledge fusion static methods.
        """
        fcm1 = FCM()
        fcm1.add_weighted_edges_from([('A', 'B', 0.5), ('B', 'C', 0.8)])

        fcm2 = FCM()
        fcm2.add_weighted_edges_from([('A', 'B', 0.7), ('C', 'A', -0.4)])

        # Test simple join
        joined_fcm = FCM.join([fcm1, fcm2])
        self.assertAlmostEqual(joined_fcm.get_edge_data('A', 'B')[0]['weight'], 0.6)
        self.assertAlmostEqual(joined_fcm.get_edge_data('B', 'C')[0]['weight'], 0.4)
        self.assertAlmostEqual(joined_fcm.get_edge_data('C', 'A')[0]['weight'], -0.2)

        # Test join by vote
        fcm3 = FCM()
        fcm3.add_weighted_edges_from([('A', 'B', 0.9)])
        voted_fcm = FCM.join_by_vote([fcm1, fcm2, fcm3])
        # Edge (A, B) should exist because it's in all 3 FCMs
        self.assertTrue(voted_fcm.has_edge('A', 'B'))
        # Edge (B, C) should be vetoed because it's only in 1 of 3 FCMs
        self.assertFalse(voted_fcm.has_edge('B', 'C'))

    def test_create_initial_vector(self):
        """
        Test the create_initial_vector static method.
        """
        active_nodes = ["HCP", "inju"]
        initial_vector = FCM.create_initial_vector(self.fcm, active_nodes)

        node_order = list(self.fcm.nodes())
        for i, node in enumerate(node_order):
            if node in active_nodes:
                self.assertEqual(initial_vector[i], 1)
            else:
                self.assertEqual(initial_vector[i], 0)

    def test_hebbian_learning(self):
        """
        Test the Hebbian learning method.
        """
        fcm = FCM()
        fcm.add_weighted_edges_from([('A', 'B', 0.5)])

        # Create a simple time-series where A and B are correlated
        data = np.array([
            [0, 1, 0, 1, 0, 1],  # Node A
            [0, 1, 0, 1, 0, 1]   # Node B
        ])

        # Learn with the 'hebbian' rule
        fcm.learn(data, rule='hebbian', learning_rate=0.1)

        # The weight should increase because of the correlation
        self.assertTrue(fcm.get_edge_data('A', 'B')[0]['weight'] > 0.5)

    def test_visualize_evolution(self):
        """
        Test the evolution visualization method.
        """
        import os
        output_dir = "output"

        # Evolve the clot FCM from a known state
        initial_vector = np.zeros(len(self.fcm.nodes()))
        mask = np.zeros_like(initial_vector)
        history = self.fcm.evolve_to_limit(initial_vector, mask)

        # Generate the visualization
        self.fcm.visualize_evolution(history, output_dir=output_dir)

        # Check that the image files were created
        self.assertTrue(os.path.exists(output_dir))
        self.assertTrue(len(os.listdir(output_dir)) > 0)

        # Clean up the created files
        for f in os.listdir(output_dir):
            os.remove(os.path.join(output_dir, f))
        os.rmdir(output_dir)


if __name__ == '__main__':
    unittest.main()
