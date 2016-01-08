Directed acyclic word graphs
============================


The library implements *directed acyclic word graphs* (DAWGs)
internally represented as *minimal acyclic deterministic finite-state
automata*.

The library allows to build DAWGs from words over any alphabet
providing an `Ord` instance.
It also provides a fast insert operation which can be used to
build DAWGs on-the-fly.
