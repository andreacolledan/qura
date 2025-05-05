# Extending QuRA

All the logic for the analysis of a specific resource metric is encoded as a *metric module*, that is, an instance of `GlobalMetricModule` (for global metrics) or `LocalMetricModule` (for local metrics).
This guide is meant to aid contributors in the implementation of new metric modules, that is, in the extension of QuRA with the ability tackle new definitions of "resource".

## The `Index` datatype

The definition of a new metric analysis boils down to specifying *what resource annotations should be produced* when analyzing a small number of key circuit building scenarios. These annotations are called *indices*, and they are essentially arithmetic expressions built from natural numbers and index variables. They are represented through the `Index` datatype, in the [`PQ.Index`](https://github.com/andreacolledan/qura/blob/main/src/PQ/Index.hs) module. 

Here is a list of the `Index` constructors that can be used when defining new metrics:

- `Number :: Int -> Index` makes an index out of an integer. For matters of soundness, only non-negative integers should appear in indices.
- `IVar :: IVarId -> Index` makes an index variable out of a string identifier.
- `Plus :: Index -> Index -> Index` represents the sum of two index expressions.
- `Minus :: Index -> Index -> Index` represents the natural subtractions of two index expression. Note that `Minus e1 e2` is equal to zero whenever `e1` is less or equal than `e2`.
- `Mult :: Index -> Index -> Index` represents the product of two index expressions.
- `Max :: Index -> Index -> Index` represents the maximum of two index expressions.
- `BoundedSum :: IVarId -> Index -> Index` represents the bounded sum of indices. `BoundedSum "i" e1 e2` is the sum for `i` going from 0 (included) to `e1` (excluded) of `e2`, where `e2` can depend on `i`.
- `BoundedMax :: IVarId -> Index -> Index` represents the bounded maximum of indices. `BoundedMax "i" e1 e2` is the maximum for `i` going from 0 (included) to `e1` (excluded) of `e2`, where `e2` can depend on `i`.

## Global metrics

### Defining a global metric module

An instance of `GlobalMetricModule` requires the definition of the following functions:

- `name :: String` is just the name of the metric, used for pretty printing.
- `desugarIdentity :: Index` defines the neutral element for the metric (e.g. what it means to have "no width"). This is usually just `Number 0`.
- `desugarWire :: WireType -> Index` defines the size of a wire, which might depend on its wiretype. Note that `WireType = Bit | Qubit`. For example, in the case of width:
```hs
-- src/Metric/Global/Width.hs

desugarWire Bit = Number 1
desugarWire Qubit = Number 1
```

- `desugarOperation :: QuantumOperation -> Index` defines the size of each elementary quantum operation available in PQR. The list of of available `QuantumOperation`s is available in the [`Circuit`](https://github.com/andreacolledan/qura/blob/main/src/Circuit.hs) module. For example, for T-count:
```hs
-- src/Metric/Global/TCount.hs

desugarOperation T = Number 1
desugarOperation _ = Number 0
```

- `desugarSequence :: Index -> Index -> Index` defines the size of two circuits composed in sequence. Suppose `C` is a circuit of size `e1` and `D` is a circuit of size `e2`. Then, `desugarSequence e1 e2` should be the size of the composition in sequence of `C` and `D`. For example, in the case of width:
```hs
-- src/Metric/Global/Width.hs

desugarSequence e1 e2 = Max e1 e2
```


- `desugarParallel :: Index -> Index -> Index` defines the size of two circuits composed in parallel. Suppose `C` is a circuit of size `e1` and `D` is a circuit of size `e2`. Then, `desugarSequence  e1 e2` should be the size of the composition in parallel of `C` and `D`. For example, in the case of width:
```hs
-- src/Metric/Global/Width.hs

desugarParallel e1 e2 = Plus e1 e2
```

- `desugarBoundedSequence :: IVarId -> Index -> Index -> Index` is the same as `desugarSequence`, but generalized to bounded sequences. That is, `desugarBoundedSequence "i" e1 e2` is the size of the circuit obtained by composing in sequence `e1` circuits, where the size of the `i`-th circuit is `e2` and might depend on `i`.
```hs
-- src/Metric/Global/Width.hs

desugarBoundedSequence i e1 e2 = BoundedMax i e1 e2
```

- `desugarBoundedParallel :: IVarId -> Index -> Index -> Index` is the same as `desugarParallel`, but generalized to bounded sequences. That is, `desugarBoundedParallel "i" e1 e2` is the size of the circuit obtained by composing in parallel `e1` circuits, where the size of the `i`-th circuit is `e2` and might depend on `i`.
```hs
-- src/Metric/Global/Width.hs

desugarBoundedParallel i e1 e2 = BoundedSum i e1 e2
```

For more examples of global metric modules, consult the modules under [`Metric.Global`](https://github.com/andreacolledan/qura/tree/main/src/Metric/Global)

### Making it available through QuRA's interface

In order to make a new `GlobalMetricModule` available from QuRA's interface, you need to first re-export it in [`Metric.hs`](https://github.com/andreacolledan/qura/blob/main/src/Metric.hs) and then make the following changes to [`Main.hs`](https://github.com/andreacolledan/qura/blob/main/app/Main.hs):

1. Add a new case to `globalMetricArgParser` to parse your new metric as a command line option.
2. Add your metric's name to the error message of the argument parser, so that users know it's there.

If your metric is defined in `myGlobalMetricModule` and is called `myglobalmetric`, the last lines of the case analysis in `globalMetricArgParser` should look like this:

```
globalMetricArgParser = do
  s <- str 
  case s of
  ...
  "myglobalmetric" -> return myGlobalMetricModule
  _ -> readerError "Supported global resources are 'width', 'gatecount', 'qubits', 'bits', 'tcount', `myglobalmetric`"

```

Finally, to analyze `myglobalmetric` in QuRA, run
```
qura FILE -g myglobalmetric
```

## Local metrics

### Defining a local metric module

An instance of `LocalMetricModule` requires the definition of the following functions:

- `name :: String` is just the name of the metric, used for pretty printing.
- `desugarOutput :: QuantumOperation -> Int -> [Index] -> Index` describes the local metric associated to each of the outputs of a quantum operation, as a function of the operation itself and the local metrics of its inputs. `desugarOutput op n inMetrics` is the local metric associated to the `n`-th output of `op`, when the local metrics associated to its inputs are those in `inMetrics`. For example, for depth:

```hs
-- src/Metric/Local/Depth.hs

desugarOutput _ _ inDepths =
  foldr Max (Number 0) $ map (Number 1 `Plus`) inDepths
```

For more examples of local metric modules, consult the modules under [`Metric.Local`](https://github.com/andreacolledan/qura/tree/main/src/Metric/Local)

### Making it available through QuRA's interface

In order to make a new `LocalMetricModule` available from QuRA's interface, you need to first re-export it in [`Metric.hs`](https://github.com/andreacolledan/qura/blob/main/src/Metric.hs) and then make the following changes to [`Main.hs`](https://github.com/andreacolledan/qura/blob/main/app/Main.hs):

1. Add a new case to `localMetricArgParser` to parse your new metric as a command line option.
2. Add your metric's name to the error message of the argument parser, so that users know it's there.

If your metric is defined in `myLocalMetricModule` and is called `mylocalmetric`, the last lines of the case analysis in `localMetricArgParser` should look like this:

```hs
localMetricArgParser = do
  s <- str 
  case s of
  ...
  "mylocalmetric" -> return myLocalMetricModule
  _ -> readerError "Supported local resources are 'depth', `tdepth`, `mylocalmetric`"
```

Finally, to analyze `mylocalmetric` in QuRA, run
```
qura FILE -l mylocalmetric
```