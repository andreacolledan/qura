## PQR Documentation (WIP)

QuRA takes as input programs written in a variant of Quipper called PQR (short for Proto-Quipper-R). This language is based on the Proto-Quipper family of theoretical programming languages. As such, PQR is, at its core, a lambda calculus with bespoke constructs to describe and manipulate quantum circuits, dressed in a Haskell-like syntax.

## Table of contents

- [PQR Documentation (WIP)](#pqr-documentation-wip)
- [Table of contents](#table-of-contents)
- [PQR Basics](#pqr-basics)
  - [Effect typing](#effect-typing)
  - [Linear types](#linear-types)
  - [Refinement types](#refinement-types)
- [Language Features](#language-features)
  - [Qubits and bits](#qubits-and-bits)
  - [Applying operations to wires](#applying-operations-to-wires)
    - [Example](#example)
  - [Linear functions](#linear-functions)
    - [Examples](#examples)
      - [Defining functions](#defining-functions)
      - [Function application](#function-application)
  - [Lifting and forcing](#lifting-and-forcing)
    - [Example](#example-1)
      - [Lifting](#lifting)
      - [Forcing](#forcing)
  - [Circuit boxing](#circuit-boxing)
    - [Examples](#examples-1)
      - [Boxing circuits](#boxing-circuits)
      - [Applying boxed circuits](#applying-boxed-circuits)
  - [Annotation polymorphism](#annotation-polymorphism)
  - [Sized dependent lists](#sized-dependent-lists)
    - [Examples](#examples-2)
      - [Size](#size)
      - [Dependency](#dependency)
  - [Fold](#fold)
- [Primitive Operations](#primitive-operations)
  - [Qubit and Bit meta-operations](#qubit-and-bit-meta-operations)
  - [Single-qubit gates](#single-qubit-gates)
  - [Controlled gates](#controlled-gates)
  - [Classically controlled gates](#classically-controlled-gates)
  - [Convenience gates](#convenience-gates)
  - [Other primitives](#other-primitives)

## PQR Basics

In PQR, programs contain both classical and quantum operations. However, only classical operations are actually executed, while quantum operations are buffered to a *quantum circuit*, with the idea that the latter can then be further manipulated as a whole, or sent to quantum hardware for execution.
When we talk of the resource requirements of a PQR program, we are really just talking about the size of this quantum circuit, which might depend on the classical inputs to the program.

PQR obeys an effectful, linear-nonlinear, refinement typing discipline. Let us break this down.

### Effect typing

PQR builds a circuit as a side-effect of a computation. When dealing with effectful functional programming languages, in addition to giving types to programs, we can also give them *[effect annotations](https://en.wikipedia.org/wiki/Effect_system)*, which describe the side-effect they produce.

In the case of QuRA, effect annotations are used to describe the size of the circuits built by a program. When QuRA is run, two outputs are generally produced:

```
$ qura examples/grover.pqr -g width
* Inferred type: List[_<3] Bit
* Inferred width upper bound: 4
```
Where "inferred width upper bound" is precisely the effect annotation, which tells us that the quantum computation corresponds to a circuit of width at most 4.

### Linear types

[Linear types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems) ensure that some values of a language cannot be duplicated or discarded, that is, that they are used *exactly once*. 

In the context of quantum programming languages, this is particularly desirable when handling qubits: due to the no-cloning properties of quantum states, qubits cannot generally be duplicated. Furthermore, qubits might be entangled, meaning that losing acces to a qubit might cause a loss of information about the quantum system as a whole. As such, qubits must be explicitly (and properly) discarded, they cannot just go out of scope.

In PQR, qubit and bit references are treated linearly. Functions are also treated linearly by default. Any piece of data containing linear values is itself treated linearly.

All other values are not linear. They are called *parameters* and are freely duplicable and discardable.

### Refinement types

[Refinement types](https://en.wikipedia.org/wiki/Refinement_type) constitute an approach to verification where regular types are annotated with additional, quantitative information about the values they represent.

In QuRA, refinement types work together with effects in order to achieve precise size analysis, as base types are annotated with additional information regarding the size of the values they represent: qubit and bit types are annotated with their depth, lists are annotated with their length, boxed circuits are annotated with their size, and so on.

## Language Features

### Qubits and bits

The fundamental objects of circuit-building are qubits and bits. Because PQR is a *circuit description language*, these have to be interpreted as references to available wires in a global underlying circuit that is built incrementally as the program executes.

A qubit reference has type `Qubit{n}`, where `n` is the local metric information (e.g. depth) associated to the qubit in the circuit.
Similarly, a bit reference has type `Bit{n}`.

### Applying operations to wires

All operations that actively modify the underlying circuit are carried out through the `apply` operator:

```
apply(op, wires)
```

applies operation `op` to the qubits and bits in `wires`, and returns the wires output by `op`. Circuit operations are given *circuit types*. A circuit type like
```
Circ[n](inType, outType)
```
represents an operation that has input type `inType`, output type `outType` and size `n` within the circuit. 

All the primitive circuit operations available in PQR are described [here](#primitive-operations).

**Note:**
*All* operations that affect the underlying circuit are performed via `apply`. This includes the initialization of new bits and qubits, measurements, discardings, etc. When an operation has no inputs (e.g. qubit initialization), it is applied to the unit value `()`.

#### Example

```
let q = apply(QInit0, ()) in
let q = apply(Hadamard @ 0, q) in
apply(QDiscard @ 1, q)
```

```
$ qura examples/snippets/apply.pqr -g width -l depth
* Inferred type: ()
* Inferred width upper bound: 1
```


### Linear functions
Functions in PQR are defined through lambdas. The argument of a lambda must be annotated with its type: 
```
\arg :: type . body
```
is a function whose argument is `arg` of type `type` and whose body is `body`. A function type like
```
inType -o[n,m] outType
```
describes a function that takes an input of type `inType`, builds a circuit of size at most `n`, and returns a value of type `outType`. Annotation `m` represents the size of the wires captured within the function's closure. Although technical in nature, this annotation is essential to accurately estimate the size of the circuits described in PQR.

Functions in PQR are by default linear meaning that, without [further devices](#lifting-and-forcing), they must be applied exactly once. PQR also supports higher-order functions.

#### Examples
##### Defining functions

```
-- Swap two qubits at depth 0
let swap = \(x,y) :: (Qubit{0}, Qubit{0}) . (y,x) in
swap
```
```
$ qura examples/snippets/swap.pqr -g width -l depth
* Inferred type: (Qubit{0}, Qubit{0}) -o[2, 0] (Qubit{0}, Qubit{0})
* Inferred width upper bound: 0
```
##### Function application
```
let q = apply(QInit0, ()) in
let p = apply(QInit0, ()) in
swap (q,p)
```
```
$ qura examples/snippets/swap.pqr -g width -l depth
* Inferred type: (Qubit{0}, Qubit{0})
* Inferred width upper bound: 2
```

### Lifting and forcing

When a value does not contain linear resources, it can be made into a duplicable parameter by means of *lifting*:

```
lift expr
```
turns expression `expr` into a parameter, provided its typing does not involve linear resources. If `expr` is a computation, it is suspended. To "unwrap" a lifted value and use it like you normally would, we need to *force* it:

```
force expr
```
If `expr` has type `type`, then `lift expr` has type `![n] type`, where `n` is the size of the circuit built by `expr` once forced.

#### Example
##### Lifting

```
let qinit = lift apply(QInit0, ()) in
qinit
```
```
qura examples/snippets/lift.pqr -g width -l depth
* Inferred type: ![1]Qubit{0}
* Inferred width upper bound: 0
```
##### Forcing
```
force qinit
```
```
$ qura examples/snippets/lift.pqr -g width -l depth
* Inferred type: Qubit{0}
* Inferred width upper bound: 1
```
### Circuit boxing
All the circuit operations we have used so far are primitives of the language. However, we can turn any circuit-building function into a modular, reusable circuit operation via *boxing*:
```
box function
```
takes the circuit built by `function` and turns it into a *boxed circuit*, that is, a datum representation of the (sub)circuit built by `function`, which can then be reused, manipulated, etc.

If `function` has type `![0] (inType -o[n,0] outType)`, then `box function` has type `Circ[n](inType, outType)`.

#### Examples
##### Boxing circuits

```
let oracle = box $ lift \(target, input) :: (Qubit{0}, Qubit{0}).
  let input = (force qnot @ 0) input in
  let (target, input) = (force cnot @ 1 @ 1) input target in
  let input = (force qnot @ 2) input in
  (target, input)
in

oracle
```
```
$ qura examples/snippets/box.pqr -g width -l depth
* Inferred type: Circ[2]((Qubit{0}, Qubit{0}), (Qubit{2}, Qubit{3}))
* Inferred width upper bound: 0
```
##### Applying boxed circuits
```
let q = apply(QInit0, ()) in
let p = apply(QInit0, ()) in
apply(oracle, (q, p))
```
```
$ qura examples/snippets/box.pqr -g width -l depth
* Inferred type: (Qubit{2}, Qubit{3})
* Inferred width upper bound: 2
```

### Annotation polymorphism




### Sized dependent lists

Unlike lists in most functional programming languages, PQR lists grow to the right. The empty list is `[]`, whereas `xs:x` denotes the append of `x` to the list `xs`. List literals are also supported.

PQR lists are *sized* and *dependent*, meaning that their exact size is reflected in the type, and that the type of an element might depend on its position within the list. A *sized dependent list type* of the form
```
List[i<n] type
```
describes lists of length `n` in which the `i`-th element has a type `type` which may depend on `i`.

#### Examples
##### Size
```
[(),(),()]
```
```
$ qura examples/snippets/lists.pqr
* Inferred type: List[i<3] ()
* Inferred width upper bound: 0
```
##### Dependency
```
let empty = [] :: List[i<0](List[j<i+1] ()) in
empty:[()]:[(),()]:[(),(),()]
```
```
$ qura examples/snippets/lists.pqr -l depth
* Inferred type: List[i<3] List[j<i+1] ()
```



### Fold

## Primitive Operations

At the time of writing, the following are the primitive operations that can be used for circuit-building in PQR

### Qubit and Bit meta-operations

- `QInit0` and `QInit1` initialize a new `Qubit` to the states $|0\rangle$ and $|1\rangle$, respectively. Also available through library functions `qinit0` and `qinit1`.
- `QDiscard` discards a `Qubit`. Also available through library function `qdiscard`.
- `CInit0` and `CInit1` initialize a new `Bit` to the states $0$ and $1$, respectively. Also available through library functions `cinit0` and `cinit1`.
- `CDiscard` discards a `Bit`. Also available through library function `cdiscard`.
- `Meas` measures a `Qubit`, returning a `Bit`. Also available through library function `meas`.
  
### Single-qubit gates

- `Hadamard` performs the Hadamard transform on a single `Qubit`. Also available as library function `hadamard`.
- `PauliX` flips a single `Qubit` over the x-axis, i.e. it negates its input. Also available as library function `pauliX`.
- `PauliY` flips a single `Qubit` over the y-axis. Also available as library function `pauliY`.
- `PauliZ` flips a single `Qubit` over the z-axis. Also available as library function `pauliZ`.
- `T` rotates a single `Qubit` by π/4 radians around the z-axis. Also available as library function `tgate`.

### Controlled gates

- `CNot` takes as input a pair of `Qubit`s and negates the second (the *target*) if the first (the *control*) is $|1\rangle$. The control qubit is also returned. Also available as library function `cnot`.
- `CZ` takes as input a pair of `Qubit`s and applies `PauliZ` to the second (the *target*) if the first (the *control*) is $|1\rangle$. The control qubit is also returned. Also available as library function `cz`.
- `Toffoli` takes as input three `Qubit`s and negates the third (the *target*) if the first two (the *controls*) are $|1\rangle$. The control qubits are returned unchanged. Also available as library function `toffoli`.
- `MakeCRGate @i`, where `i` is a natural number, takes as input a pair of `Qubit`s and shifts the phase of the second (the *target*) by $2π/2^i$ radians if the first (the *control*) is $|1\rangle$. The control qubit is returned unchanged. Also available as library function `cR`.

### Classically controlled gates
- `CCNot`: like `CNot`, but the control is a classical `Bit`. Also available as library function `ccnot`.
- `CCZ`: like `CZ`, but the control is a classical `Bit`. Also available as library function `ccz`.
  
### Convenience gates
- `MakeNToffoli @n`, where `n` is a natural number, takes as input a list of `n` control `Qubit`s and a target `Qubit` and applies the `n`-controlled Toffoli gate to them. Also available as library function `mcnot`.
- `MakeNCZ @n`, where `n` is a natural number, takes as input a list of `n` control `Qubit`s and a target `Qubit` and applies the `n`-controlled Pauli-Z gate to them. Also available as library function `mcZ`.

### Other primitives
- `MakeUnitList @n`, where `n` is a natural number, retuns a list of `()` of length `n`. Useful to simulate iteration using fold. Also available as library function `range`.