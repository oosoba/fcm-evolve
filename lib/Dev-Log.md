# FCM Lib: Implementation Details

## Version 0.2 Planning
- [ ] Plot out the limit cycles
- [x] Inverse search/Backward Chaining: Given either an output state or limit cycle. Find some or all of the input states (unforced) that lead to those.  Special case of running all possible inputs and listing all output limit cycles and fixed points.
    - see socsim model for exhaustive search solution to this

## Version 0.1
- [x] Plot the FCM with at least signed edges
- [x] Synchronous FCM inference (update all neurons at each iteration)
- [x] Synchronous update with policy enforcement (nodes "clamped" on or off for all or part of inference steps).
    - Example: Clamp on concept node C1.  Let the FCM converge to a stable limit cycle (still swirling around).  THEN unclamp C1 and let the FCM converge of a new limit cycle.
- [x] Repeat (2) and (3) for subset ASYNCHRONOUS updates (or at least simple asynchrony with updating one neuron at a time)
- [x] Ability to combine several FCMs.   Compare limit cycles of individual FCMs versus combined FCM
