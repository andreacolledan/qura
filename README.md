![QuRA-Logo](Header.png)

**Disclaimer:** support for metrics other than circuit width is still experimental.

QuRA is a static analysis tool for the verification of the resource consumption of quantum circuit description programs. QuRA takes as input a program written in a variant of Quipper called [PQ](src/Lang/README.md). and outputs two things: a type for the program and an upper bound to the size of the circuit it will build.

## Getting started

Consider a PQ program that implements a function `dumbNot` defined as:
```
let dumbNot = \q :: Qubit .
  let a = force qinit1 in         -- initialize a temporary qubit to |1>
  let (a,q) = force cnot a q in   -- apply cnot between input q and temporary qubit a
  let _ = force qdiscard a        -- discard temporary qubit a
  in q                            -- return input qubit
in

dumbNot
```
This function describes a very basic quantum computation in the form of a quantum circuit: the fundamental units of data are `Qubit`s and `Bit`s and computations on these data are carried out by applying elementary operations to them. These operations are either quantum gates (e.g. `cnot`, `hadamard`, etc.), measurements, or other operations used to manage bits and qubits (e.g.`qinit1`, `qdiscard`, etc.).

### Circuit size estimation

As the name suggests, `dumbNot` implements the negation of a qubit in a dumb, needlessly expensive way. But let's forget about what the function does and let's focus instead on the circuit it builds. Applying `dumbNot` to an input qubit `q` produces the following circuit:

![dumbNot-Circuit](dumbnot-circuit.png)

In terms of width, this circuit has size 2. In terms of gate count, it has size 1. QuRA is able to automatically infer this information from the definition of `dumbNot`, without having to run the program, by giving `dumbNot` one of the following function types, depending on the chosen size metric:
```
$ qura examples/dumbNot.PQ -g width
Inferred type: Qubit -o[2,0] Qubit

$ qura examples/dumbNot.pqr -g gatecount
Inferred type: Qubit -o[1,0] Qubit
```
The linear arrow type is indexed with two numbers: the first one gives us an upper bound to the size of the circuit built by `dumbNot` according to the chosen metric, while the second one tells us the size of the circuit wires that are captured inside the function's closure (in this case, no wires are captured). The latter annotation, although technical in nature, is essential to correctly estimate circuit size in many cases.

In such a simple example, there is no incentive to *verify* this property, as opposed to *test* it: `dumbNot` always builds the same circuit, regardless of its input. We might as well have executed `dumbNot`, checked the resulting circuit and obtained the same information about its size, without the need for a type system at all.
Where QuRA really excels is in the verification of *circuit families*, that is, functions that build different circuits depending on some input parameter.
PQ supports a limited form of dependency, which is restricted to the numerical indices used to annotate types. This allows to verify entire circuit families at once, obtaining generic, parametric upper bounds that hold for *any* choice of parameters. E.g. the `qft` circuit family is implemented in PQ as follows:
```
let rotate = forall m. lift forall k.\((ctrls, trg), ctrl)::((List[j<k] Qubit, Qubit), Qubit).
    let (ctrl, trg) = (force cR @ m+1-k @ 0 @ 0) ctrl trg in
    (ctrls:ctrl, trg)
in

let qft = forall n. \reg :: List[j<n] Qubit.
  let qftStep = lift forall m.\(ctrls, trg)::(List[j<m] Qubit, Qubit).
      let revctrls = (force qrev @m) ctrls in
      let (ctrls, trg) = fold(rotate @m, ([], trg), revctrls) in
      let trg = (force hadamard @ 0) trg in
      ctrls:trg
  in fold(qftStep, [], reg)
in qft
```
Abstractions over index variables is achieved through the `forall` binder. Indices are arithmetic expressions over natural numbers and index variables and are the only kind of term that's allowed to appear in types. Note also that general recursion is not available in PQ. Instead, a limited form of recursion is made available via the primitive `fold` construct. QuRA infers the following types for `qft`:
```
qura examples/qft.pqr -g width
Inferred type: forall[0, 0] n. List[j<n] Qubit -o[n, 0] List[j<n] Qubit

qura examples/qft.pqr -g gatecount
Inferred type: forall[0, 0] n. List[j<n] Qubit -o[sum[m < n](m + 1), 0] List[j<n] Qubit
```
meaning that the QFT circuit of input size $n$ has width $n$ and is made up of at most $\sum_{m=0}^{n-1} m+1$ gates, for all $n$.

### Local resource estimation

The aforementioned notions of circuit size are applicable to a circuit as a whole, which is why we call them *global metrics*. However, there are other definitions of size, such as circuit depth, that are more local in nature, as they can be attributed to the individual wires of a circuit in a meaningful, more informative way. We call these *local metrics*. For example, the depth of a wire segment `q` is naturally defined as the maximum number of gates occurring between any of the circuit's inputs and `q`. Then, the depth of a circuit is nothing more than the maximum depth of any of its outputs.

QuRA features experimental support for the estimation of local metrics. As an example, consider the same implementation of the QFT algorithm from before, where wire types are further annotated so as to allow for the analysis of the depth of the circuit:

```
let rotate = forall i. forall m. lift forall k.
  \((ctrls, trg), ctrl)::((List[j<k] Qubit{i+m+j+1}, Qubit{i+m+k}), Qubit{i+m+k}).
    let (ctrl, trg) = (force cR @(m+1-k) @(i+m+k) @(i+m+k)) ctrl trg in
    (ctrls:ctrl, trg)
in

let qft = forall n. forall i.
  \reg :: List[j<n] Qubit{i}.
    let qftStep = lift forall m.\(ctrls, trg)::(List[j<m] Qubit{i+m+j}, Qubit{i}).
        let revctrls = (force qrev @m @i) ctrls in
        let (ctrls, trg) = fold(rotate @i @m, ([], trg), revctrls) in
        let trg = (force hadamard @(i+2*m)) trg in
        ctrls:trg
    in fold(qftStep, [], reg)
in qft
```

Wire tipes are now annotated with indices within braces, which describe the depth at which the corresponding wire sits. For example, `Qubit{0}` would represent a qubit wire at depth 0 (so a freshly initialized qubit). This implementation of the QFT algorithm takes as input a list of $n$ qubits all at depth $i$ and outputs a list of $n$ qubits where the $j$-th qubits sits at a depth of $i+n+j$.

```
$ qura examples/qft.pqr -l depth
Inferred type: forall n. forall i. List[j<n] Qubit{i} -o List[j<n] Qubit{i + n + j}
```

## Installing
**Note:** QuRA requires [cvc5](https://cvc5.github.io) to be installed and present in your `PATH`.

You can install QuRA using [stack](https://docs.haskellstack.org/en/stable/) by running

```
$ git clone https://github.com/andreacolledan/qura
$ cd qura
$ git checkout multimetric-analysis
$ stack install
```
**Disclaimer:** support for metrics other than circuit width is still experimental.

## Usage
To run QuRA on the PQ file `FILE`, run
```
$ qura FILE
```
This runs standard type inference for the program in `FILE`, without any resource estimation. To also perform global metric estimation, use the `-g METRIC` option. For example, to perform width estimation, run

```
$ qura FILE -g width
```
To perform local metric estimation, use the `-l METRIC` option instead. Note that at most one global metric and on local metric can be analyzed at a time.

Currently, qura supports the analysis of the following circuit size metrics:

| Flag | Type | Description |
|-|-|-|
| width | Global | How many individual wires (qubits and bits) required to execute the circuit |
| qubits | Global | How many individual qubits required to execute the circuit  | Global |
| bits | Global | How many individual bits required to execute the circuit
| gatecount | Global | How many gates the circuit is made of
| tcount | Global | How many T gates are in the circuit |
| depth | Local | The maximum number of gates occurring on a path from any input to the wire segment under analysis
| tdepth | Local | The maximum number of T gates occurring on a path from any input to the wire segment under analysis

Use option `--debug DEBUG` to dump a copy of all SMT queries performed during typechecking to file `DEBUG`.

General usage of qura is thus
```
qura FILE [-v | --verbose] [-d | --debug DEBUG] [-g | --global-metric-analysis METRIC] [-l | --local-metric-analysis METRIC]
```
For more information, refer to `qura --help`.

## Contributing

If you are interested in extending QuRA with new kinds of metric analysis, consult [this guide](src/Index/Semantics/README.md).
