![QuRA-Logo](Header.png)

**Disclaimer:** support for metrics other than circuit width is still experimental.

QuRA is a static analysis tool for the verification of the resource consumption of quantum circuit description programs. QuRA takes as input a program written in a variant of Quipper called Proto-Quipper-R and outputs two things: a type for the program and an upper bound to the size of the circuit it builds.
A more detailed description of QuRA's input language can be found [here](src/Lang/Unified/README.md). At the heart of QuRA lies a linear dependent type-and-effect system which is described in greater detail in [this work](https://doi.org/10.48550/arXiv.2310.19096).

## Getting started

Consider a function `dumbNot` defined as:
```
\q :: Qubit .
  let a = force qinit1 () in        -- initialize a temporary qubit
  let (a,q) = force cnot a q in     -- apply cnot between input q and temporary qubit a
  let _ = force qdiscard a          -- discard temporary qubit a
  in q                              -- return input qubit
```
This function describes a very basic quantum computation in the form of a quantum circuit: the fundamental units of data are `Qubit`s and `Bit`s and computations on these data are carried out by applying elementary operations on them. These operations are either quantum gates (e.g. `cnot`, `hadamard`, etc.) or other operations on bits and qubits (e.g.`qinit1`, `qdiscard`, `meas`, etc.).

As the name suggests, `dumbNot` implements the negation of a qubit in a dumb, unnecessarily expensive way. But let's forget about what the function does and let's focus instead on the circuit it builds. Applying `dumbNot` to an input qubit `q` produces the following circuit:

![dumbNot-Circuit](dumbnot-circuit.png)

In terms of width, this circuit has size 2. In terms of gate count, it has size 1. QuRA is able to automatically infer this information from the definition of `dumbNot`, without having to run the program, by giving `dumbNot` one of the following function types, depending on the chosen metric:
```
Qubit -o[2,0] Qubit -- for width
Qubit -o[1,0] Qubit -- for gate count
```
Each linear arrow is indexed with two numbers: the first one tells us the size of the circuit build by `dumbNot` according to the chosen metric, while the second one tells us the size of the circuit wires that are captured inside the function's closure (in this case, no wires are captured). The latter index, although exotic in nature, is essential to correctly estimate circuit metrics in many cases.

Proto-Quipper-R (QuRA's input language) supports a limited form of depdendency, which is precisely restricted to the indices used to annotate types. This allows for the description of circuit families that depend on a natural number parameter. E.g. the `qft` circuit family is implemented as follows:
```
... -- auxiliary functions

@n.\qs :: List[i<n] Qubit.
  -- define the step function
  let qftStep = lift @m.\(qs, q) :: (List[j<m] Qubit, Qubit).
      let revqs = (force qrev @m) qs in
      let (q, qs) = fold(rotate @m, (q, []), revqs) in
      let q = force hadamard q in
      qs:q
  in
  -- fold it over the input
  fold(qftStep, [], qs)
```
Abstractions over index variables is achieved through the `@` binder. Indices are arithmetic expressions over natural numbers and index variables and are the only kind of term that's allowed to appear in types. Note also that general recursion is not available in Proto-Quipper-R. Instead, a limited form of recursion is made available via the primitive `fold` construct. QuRA infers the following types for `qft`:
```
-- for width:
n ->[0, 0] (List[i < n] Qubit -o[n, 0] List[j < n] Qubit)
-- for gate count:
n ->[0, 0] (List[i < n] Qubit -o[sum[m < n] (m + 1), 0] List[j < n] Qubit)
```
meaning that for every `n`, `qft` takes as input `n` qubits and builds a circuit of width at most `n` comprising of at most $\sum_{m=0}^{n-1} m+1$ gates.

The whole code for `qft`, as well as other examples, can be found in the `examples` directory. 

## Installing
Note: QuRA requires [cvc5](https://cvc5.github.io) to be installed and present in your `PATH`.
You can then install the tool by running

```
$ git clone https://github.com/andreacolledan/qura
$ cd qura
$ stack install
```

## Usage
To run QuRA on program file `FILE`, run
```
$ qura FILE
```
This runs standard type inference for the program in `FILE`, without any resource estimation. To also perform resource estimation, add the `-g RESOURCE` option. For example, to perform width estimation, run

```
$ qura FILE -g width
```

Currently, qura supports the analysis of the following resource metrics:

| Flag | Description |
|-------|------------|
| width | How many individual wires (qubits and bits) required to execute the circuit |
| qubits | How many individual qubits required to execute the circuit  |
| bits | How many individual bits required to execute the circuit
| gatecount | How many gates the circuit is made of
| tcount | How many T gates are in the circuit |

Use option `--debug DEBUG` to dump a copy of all SMT queries performed during typechecking to file `DEBUG`.

General usage of qura is thus
```
qura FILE [-v | --verbose] [-d | --debug DEBUG] [-g | --global-resource-analysis RESOURCE]
```
For more information, refer to `qura --help`.

## Tests
To execute unit tests, run `stack test`