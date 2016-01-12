Directed acyclic word graphs
============================
[![Build Status](https://travis-ci.org/kawu/dawg-ord.svg)](https://travis-ci.org/kawu/dawg-ord)


The library implements *directed acyclic word graphs* (DAWGs) internally
represented as *minimal acyclic deterministic finite-state automata*.
The implemented version of DAWG can be seen as a map from
sequences of alphabet symbols (keys) to values.

The library allows to build DAWGs over any symbols and values
provided that they both have `Ord` instances.
It also provides a fast insert operation which can be used to
construct DAWGs on-the-fly.
