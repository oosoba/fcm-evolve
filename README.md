# Fuzzy Cognitive Map (FCM) Modeling Library
> (implementations in Python and Mathematica)

This code repo provides functions for and examples of fuzzy cognitive map (FCM) modeling. The focus is on the following FCM functions:
  - Representation: using graph structures and adjacency matrices,
  - Visualization: using network diagrams,
  - Evolution: using non-linear iterative transformations,
  - FCM Combination: combining different FCMs into a unified map via averaging or other aggregation methods,
  - Learning/Adaptation: applying Hebbian learning (& variants) to learn FCM structures and parameters.

This repository has been refactored to focus on a modernized and robust Python implementation.

## Features

- **FCM Creation**: Easily create FCMs with weighted edges using `networkx`.
- **Evolution**: Simulate the FCM's evolution over time to find fixed points or limit cycles.
- **Flexible Activation Functions**: Use predefined activation functions (step, sigmoid, tanh) or provide your own.
- **Network Analysis**: Analyze the structure of your FCM with built-in centrality metrics.
- **Visualization**: Draw your FCM using `matplotlib`.

## Quick Start

The core of the library is the `FCM` class in `lib/fcm.py`. Here's a quick example:

```python
import numpy as np
from lib.fcm import FCM
import networkx as nx

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
```
For a more detailed example, see the [blood clot formation model example notebook](./examples/clot_fcm_example.ipynb).

## Associated Publications
- [**JDMS**] Fuzzy Cognitive Maps of Public Support for Terrorism ([link](https://journals.sagepub.com/doi/pdf/10.1177/1548512916680779)).
    - see [PSOT folder](./PSOT-[JDMS]/) for FCM specification and simulation details
- [**Social Sim.**] Causal Modeling with Feedback Fuzzy Cognitive Maps ([link to abridged vers.](https://onlinelibrary.wiley.com/doi/abs/10.1002/9781119485001.ch25)).
    - see [Thucydides Trap folder](./ThucydidesTrap-[SocSim]/) for FCM specification and simulation details

This repository also contains some sample specifications for FCMs from prior literature. E.g. for blood clot formation (discussed in [Taber et. al.](https://dl.acm.org/citation.cfm?id=1190436)).
