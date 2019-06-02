# Fuzzy Cognitive Map (FCM) Modeling Library
> (implementations in Python and Mathematica)

This code repo provides functions for and examples of fuzzy cognitive map (FCM) modeling. The focus is on the following FCM functions:
  - Representation: using graph structures and adjacency matrices,
  - Visualization: using network diagrams,
  - Evolution: using non-linear iterative transformations,
  - FCM Combination: combining different FCMs into a unified map via averaging or other aggregation methods,
  - Learning/Adaptation: applying Hebbian learning (& variants) to learn FCM structures and parameters.

The initial functions and models were built in `Mathematica`. Later models were built with a `Python` version of the FCM library. So some of the more advanced functionality show up in the Mathematica library first (e.g. asynchronous map evolution, knowledge fusion, & Hebbian learning).

Key functions and classes for both implementations in the [lib](./lib/) folder. Older [checkpoint versions](./lib/old-vers/) retained for replication purposes.

## Associated Publications
- [**JDMS**] Fuzzy Cognitive Maps of Public Support for Terrorism ([link](https://journals.sagepub.com/doi/pdf/10.1177/1548512916680779)).
    - see [PSOT folder](./PSOT-[JDMS]/) for FCM specification and simulation details
- [**Social Sim.**] Causal Modeling with Feedback Fuzzy Cognitive Maps ([link to abridged vers.](https://onlinelibrary.wiley.com/doi/abs/10.1002/9781119485001.ch25)).
    - see [Thucydides Trap folder](./ThucydidesTrap-[SocSim]/) for FCM specification and simulation details

This repository also contains some sample specifications for FCMs from prior literature. E.g. for blood clot formation (discussed in [Taber et. al.](https://dl.acm.org/citation.cfm?id=1190436)).
