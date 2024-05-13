![QuRA-Logo](Header.png)

QuRA is a static analysis tool for the verification of the resource consumption of circuit-building quantum programs. QuRA takes as input a program written in a variant of Quipper called Proto-Quipper-R and outputs two things: a type for the program and an upper bound to the width of the circuit it builds.
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

As the name suggests, `dumbNot` implements the negation of a qubit in a dumb, unnecessarily expensive way. But let's forget about what the function does and let's focus instead on the circuit it builds. Applying `dumbNot` to an input qubit `q` produces the following circuit of width 2:

![dumbNot-Circuit](dumbnot-circuit.png)

QuRA is able to automatically derive the width of this circuit from the definition of `dumbNot`, without having to run the program, by inferring for it the following function type:
```
Qubit -o[2,0] Qubit
```
The linear arrow is indexed with two numbers: the first one tells us the width of the circuit build by `dumbNot` once applied to an argument (in this case, 2), while the second one tells us the number of wire references that are captured inside the function's closure (in this case, no wires are captured). The latter index, although exotic in nature, is essential to correctly estimate circuit width in many cases.

Proto-Quipper-R (QuRA's input language) supports a limited form of depdendency, which is precisely restricted to the indices used to annotate types. This allows for the description of circuit families that depend on a natural number parameter. E.g. the `qft` circuit family is implemented as follows:
```
... -- auxiliary functions

@i.\qs :: List[i] Qubit.
  let qftStep = lift @j.\(qs, q) :: (List[j] Qubit, Qubit). -- define the step function
    let revqs = ((force qrev) @j) qs in
    let (q, qs) = fold(rotate @j, (q, []), revqs) in
    let q = force hadamard q in
    q:qs
  in fold(qftStep, [], qs)                                  -- fold it over the input list
```
Abstractions over index variables is achieved through the `@` binder. Indices are arithmetic expressions over natural numbers and index variables and are the only kind of term that's allowed to appear in types. Note also that general recursion is not available in Proto-Quipper-R. Instead, a limited form of recursion is made available via the primitive `fold` construct. QuRA infers the following type for `qft`:
```
i ->[0,0] List[i] Qubit -o[i,0] List[i] Qubit
```
meaning that for every `i`, `qft` takes as input `i` qubits and builds a circuit of width at most `i` that outputs a list of `i` qubits.

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
Use option `--debug DEBUG` to dump a copy of all SMT queries performed during typechecking to file `DEBUG`. General usage is thus
```
qura FILE [-v | --verbose] [-d | --debug DEBUG]
```
For more information, refer to `qura --help`.

## Paper
The implementation of QuRA and its input language are based on ["Colledan, A. and Lago, U.D. 2023. Circuit Width Estimation via Effect Typing and Linear Dependency (Long Version). arXiv."](https://doi.org/10.48550/arXiv.2310.19096)

## Tests
To execute unit tests, run `stack test`