# CAMP: Cost-Aware Multiparty Session Protocols

This is the artifact for the paper *CAMP: Cost-Aware Multiparty Session Protocols*.
The artifact comprises:

- A library for specifying cost-aware multiparty protocols.
- The raw data used for comparing the cost models with real execution costs.
- The cost-aware protocol specifications of the benchmarks that we studied.

The library for specifying cost-aware protocols also provides functions for
extracting cost equations from them, and for estimating recursive protocol
latencies (i.e. average cost per protocol iteration). We provide a script for
running the protocol estimations using the current parameters.

## Repository Contents

* **README.md**: This file
* **LICENSE**: BSD-3 license
* **src**: Source code of the cost-aware multiparty protocol library
* **app**: Script used for estimating sequential costs from benchmarking data
* **examples**: Cost-aware protocols used in the *CAMP* paper
* **benchmark_data**: Real execution times used in the comparison against the
  cost models.
* **run.sh**: Script to gather the costs of all the examples, using the current
  input parameters.

## Overview

**camp** is the implementation of the cost-aware multiparty protocols described
in *CAMP: Cost-Aware Multiparty Session Protocols*.  The implementation is a
Haskell library, that provides a monadic interface for specifying multiparty
protocols.

### Implementation

The source code is under *src*, and the main modules are:

- `Language.SessionTypes.Common`: common definitions for Global and Local Types.

- `Language.SessionTypes.Global`: global types.

- `Language.SessionTypes.Local`: local types.

- `Language.SessionTypes.Cost`: cost-aware global types.

- `Language.SessionTypes.Seq`: functions to instantiate sequential costs from
  benchmarking data.

### Cost-Aware Protocol Definitions

Directory *examples* contains the cost-aware protocol definitions used in the
*CAMP* paper. For example, consider the following definition in **OCamlGT.hs**.

```haskell
rpingpong :: CGT
rpingpong = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 1 $ \x -> do
    message p q (Var "\\tau_1") (CVar "c_1")
    message q p (Var "\\tau_2") (CVar "c_2")
    x
```

In this definition, `CGT` is the type of cost-aware global types. Function
`gclose :: GTM () -> CGT` takes a global type script (type `GTM ()`), and
produces a cost-aware global type. A global type script is a monadic computation
that describes a protocol. Action `mkRole` creates a new participant in the
protocol, `grec n $ \x -> do G` specifies a recursive protocol `G`, where `x` is
the recursion variable, and `n` is the annotation the amount of iterations that
the protocol is expected to run on average.

Variables `\\tau_1` and
`c_1` represent type and cost variables, that will occur in the cost expressions
extracted from this protocol. To extract cost equations, we use functions:

```haskell
throughput :: CGT -> Time
cost :: CGT -> Time
```

Cost equations can be instatiated by providing: (a) instantiations for the
communication costs, and (b) instantiation of cost and size variables. By
instantiating the equations, we obtain a mapping from participants to execution
times. We provide function `evalTime` for this purpose:

```haskell
evalTime :: Map (Role, Role) (Double -> Double)
         -> Map (Role, Role) (Double -> Double)
         -> Map String Double
         -> Time
         -> Map Role Double
```

## Benchmarks

The benchmarking data and code (or link to the original source code) used in
this paper can be found in directory *benchmarks*. Each benchmark has a
different structure. For example, directory `c-pthread/benchmarks/FFT/data/t_12`
contains the benchmarking data for running `FFT` on a 12-core machine. The file
has the following structure:


```
...
size: 4194304
	K: seq
		mean: 7.182371
		stddev: 0.048188
	K: 0
		mean: 7.283062
		stddev: 0.097863
	K: 1
		mean: 7.250648
		stddev: 0.116543
	K: 2
		mean: 3.503649
		stddev: 0.042035
	K: 3
		mean: 1.752671
		stddev: 0.039644
	K: 4
		mean: 0.944878
		stddev: 0.018909
	K: 5
		mean: 0.673194
		stddev: 0.017714
	K: 6
		mean: 0.594781
		stddev: 0.010795
	K: 7
		mean: 0.621905
		stddev: 0.009588
	K: 8
		mean: 0.717387
		stddev: 0.015388
...
```

This file gathers the real execution times for FFT on a 12-core machine. Each
entry contains the input size (e.g. 4194304), and the measured execution times
with different butterfly topology sizes (2^`K`, for the cases where `K` is not
`seq`), as well as the sequential costs (case `K: seq`).

These real costs are compared against the costs predicted from the cost-aware
protocols.

### Cost-Aware Protocols

We list the cost-aware protocols used in the paper, under directory *examples*.
These are:

- **

### Benchmarking Data

