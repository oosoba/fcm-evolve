# Fuzzy Cognitive Map (FCM) Modeling Library

A Python library for creating, analyzing, and visualizing Fuzzy Cognitive Maps (FCMs). This library provides a simple and powerful interface for modeling complex causal systems.

This repository is a modernized and refactored version of the original codebase, focusing on a robust, object-oriented Python implementation.

## Features

- **FCM Creation**: Easily create FCMs with weighted edges.
- **Evolution**: Simulate the FCM's evolution over time to find fixed points or limit cycles.
- **Flexible Activation Functions**: Use predefined activation functions (step, sigmoid, tanh) or provide your own.
- **Network Analysis**: Analyze the structure of your FCM with built-in centrality metrics (in-degree, out-degree, betweenness).
- **Visualization**: Draw your FCM using `matplotlib` and `networkx`.

## Quick Start

The core of the library is the `FCM` class, which inherits from `networkx.MultiDiGraph`. Here's a quick example of how to use it:

```python
import numpy as np
from lib.fcm import FCM

# 1. Define the FCM structure
edges = [(1, 2, 0.4), (1, 3, 1), (2, 3, 0.5), (3, 2, 0.4)]
mapping = {1: 'A', 2: 'B', 3: 'C'}

# 2. Create the FCM
my_fcm = FCM("My First FCM")
my_fcm.add_weighted_edges_from(edges)
nx.relabel_nodes(my_fcm, mapping, copy=False)

# 3. Evolve the system
initial_state = np.array([1, 0, 0])
mask = np.zeros_like(initial_state)
history = my_fcm.evolve_to_limit(initial_state, mask)

print("Evolution History:")
print(history)

# 4. Analyze the FCM
analysis = my_fcm.analyze()
print("\\nCentrality Analysis:")
print(analysis['in_degree'])
```

For a more detailed example, see the [blood clot formation model example notebook](./examples/clot_fcm_example.ipynb).

## Associated Publications

This library is based on work that has been used in the following publications:

- [**JDMS**] Fuzzy Cognitive Maps of Public Support for Terrorism ([link](https://journals.sagepub.com/doi/pdf/10.1177/1548512916680779)).
- [**Social Sim.**] Causal Modeling with Feedback Fuzzy Cognitive Maps ([link to abridged vers.](https://onlinelibrary.wiley.com/doi/abs/10.1002/9781119485001.ch25)).
