## PQR Documentation (WIP)

QuRA takes as input programs written in a variant of Quipper called PQR (short for Proto-Quipper-R). This language is based on the Proto-Quipper family of theoretical programming languages. As such, PQR is, at its core, a lambda calculus with bespoke constructs to describe and manipulate quantum circuits, dressed in a Haskell-like syntax.

## Table of contents

- [PQR Documentation (WIP)](#pqr-documentation-wip)
- [Table of contents](#table-of-contents)
- [PQR Basics](#pqr-basics)
  - [Effect typing](#effect-typing)
  - [Linear types](#linear-types)
  - [Refinement types](#refinement-types)
  - [Index terms](#index-terms)
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
  - [Dependent functions](#dependent-functions)
    - [Example](#example-2)
      - [Polymorphism in local metrics](#polymorphism-in-local-metrics)
      - [Polymorphism in list length](#polymorphism-in-list-length)
  - [Sized dependent lists](#sized-dependent-lists)
    - [Examples](#examples-2)
      - [Size](#size)
      - [Dependency](#dependency)
  - [Fold](#fold)
    - [Examples](#examples-3)
      - [Iterate on range](#iterate-on-range)
      - [Fold over list](#fold-over-list)
- [Primitive Circuit Operations](#primitive-circuit-operations)
  - [Qubit and Bit meta-operations](#qubit-and-bit-meta-operations)
  - [Single-qubit gates](#single-qubit-gates)
  - [Controlled gates](#controlled-gates)
  - [Classically controlled gates](#classically-controlled-gates)
- [Standard Library](#standard-library)
  - [Qubit and Bit meta-operations](#qubit-and-bit-meta-operations-1)
  - [Single-qubit gates](#single-qubit-gates-1)
  - [Controlled gates](#controlled-gates-1)
  - [Parametric gates](#parametric-gates)
  - [Other](#other)

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

### Index terms
Both refinements and effect annotations are essentially arithmetic expressions over natural numbers and variables. We call these expressions and variables *indices* and *index variables*, respectively.

**Note**: Indices are the *only* language terms that are allowed to appear in type refinement annotations.

Index operations include simple arithmetic (addition, natural subtraction, multiplication, maximum) as well as bounded sums and maxima: `sum[i<n] expr` is the sum for `i` going from 0 (included) to `n` (excluded) of index `expr`, which might depend on `i`. Similarly, `max[i<n] expr` is the maximum for `i` going from 0 to `n` of index `expr`.

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

All the primitive circuit operations available in PQR are described [here](#primitive-circuit-operations).

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
$ qura examples/snippets/lift.pqr -g width -l depth
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

### Dependent functions

While abstracting over regular variables gives rise to (linear) functions, abstracting over index variables gives rise to *dependent functions*:
```
forall i. body
```
abstracts `i` within `body`. Dependent functions are applied to indices using the `@` binary operator:
```
function @ index
```

Although the syntax is fairly different from that of regular functions, it helps to think of dependent functions as such. A dependent function type of the form
```
forall[n,m] i. type
```
thus represents a function that takes as input an index `index` and returns a value of type `type` which can also depend on `index`. The role of `n` and `m` is the same as in linear functions.
Note that because indices can appear in types, dependent functions are the mechanism through which parametric polymorphism is achieved in PQR.

Dependent functions are essential when dealing with [list functions](#fold).

#### Example
##### Polymorphism in local metrics

```
-- apply the Hadamard gate to a qubit at depth d
let hadamard = forall d.
  \q :: Qubit{d}. apply(Hadamard @ d, q)
in
hadamard
```
```
$ qura examples/snippets/dependency.pqr -g width -l depth
* Inferred type: forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1}
* Inferred width upper bound: 0
```
##### Polymorphism in list length
```
-- negate the last qubit in a list of n+1 qubits at depth d
let negateFirst = lift forall n. forall d.
  \list :: List[_<n+1] Qubit{d}.
    let (qs:q) = list in
    let q = (force qnot @ d) q in
    (qs, q)
in
negateFirst
```
```
qura examples/snippets/dependency.pqr -g width -l depth            
* Inferred type: ![0](forall[0,0] n. forall[0,0] d. List[_ < n+1] Qubit{d} -o[n+1, 0] (List[_<n] Qubit{d}, Qubit{d+1}))
* Inferred width upper bound: 0
```



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
PQR does not support full-fledged recursion. However, you can iterate over lists using the built-in `fold` construct:
```
fold f acc list
```
folds a duplicable step function `f` over `list`, starting with accumulator `acc`. The step function is supposed to be polymorphic in an index variable that represents the iteration of the fold. If the accumulator's type depends on this variable, the step function must increase it by exactly 1 at each iteration.

More techically, let `{expr/i}` denote the substitution of arithmetic expression `expr` for index variable `i`. Whenever
  -  `list` has type `List[i<n] elemTyp`,
  -  `f` has type `![0] (forall step. (accType, elemType{n-(step+1)/i}) -o[m,0] accType{step+1/step})`,
  -  `acc` has type `accType{0/step}`,

`fold f acc list` has type `accType{n/step}`.

**Note:** Sometimes there is no list to fold over, but it is still necessary to iterate some function `n` times. In these cases, iteration can be simulated by folding the step function over a list of unit values of length `n`. Such a list can be generated using the `range` library function, of type `forall n. List[_<n] ()`.

#### Examples
##### Iterate on range
```
-- initialize 8 qubits to |0>
let qinitStep = lift forall step.
  \(qs,_) :: (List[_<step] Qubit{0}, ()).
    qs:(force qinit0)
in
let qubits = fold(qinitStep, [], range @ 8) in
qubits
```
```
$ qura examples/snippets/fold.pqr -g width -l depth
* Inferred type: List[_<8] Qubit{0}
* Inferred width upper bound: 8
```
##### Fold over list
```
-- apply the Hadamard gate to n qubits at depth d
let hadamardMany = lift forall n. forall d.
  \x :: List[_<n] Qubit{d}.
    let hadamardStep = lift forall step.
      \(qs,q) :: (List[_<step] Qubit{d+1}, Qubit{d}).
        qs:((force hadamard @ d) q)
  in fold(hadamardStep, [], x)
in
(force hadamardMany @ 8 @ 0) qubits
```
```
$ qura examples/snippets/fold.pqr -g width -l depth
* Inferred type: List[_<8] Qubit{1}
* Inferred width upper bound: 8
```

## Primitive Circuit Operations

At the time of writing, the following are the primitives that can be used for circuit-building in PQR. 

**Note**: Although it is possible to build circuits by directly `apply`ing the following operations to the underlying circuit, it is strongly recommended to use the [corresponding wrapper library functions](#standard-library) instead.

### Qubit and Bit meta-operations

- `QInit0` and `QInit1` initialize a new `Qubit` to the states $|0\rangle$ and $|1\rangle$, respectively.
- `QDiscard @ i` discards a `Qubit` with local metric `i`.
- `CInit0` and `CInit1` initialize a new `Bit` to the states $0$ and $1$, respectively.
- `CDiscard @ i` discards a `Bit` with local metric `i`.
- `Meas @ i` measures a `Qubit` with local metric `i`, returning a `Bit`.
  
### Single-qubit gates

- `Hadamard @ i` applies the Hadamard transform to a single `Qubit` with local metric `i`.
- `PauliX @ i` flips a single `Qubit` with local metric `i` over the x-axis, i.e. it negates its input.
- `PauliY @ i` flips a single `Qubit` with local metric `i` over the y-axis.
- `PauliZ @ i` flips a single `Qubit` with local metric `i` over the z-axis.
- `T @ i` rotates a single `Qubit` with local metric `i` by π/4 radians around the z-axis.

### Controlled gates

- `CNot @i0 @i1` takes as input a *control* `Qubit` with local metric `i0` and a *target* `Qubit` with local metric `i1` and negates the target if the control is $|1\rangle$.
- `CZ` takes as input a *control* `Qubit` with local metric `i0` and a *target* `Qubit` with local metric `i1` and applies the `PauliZ` gate to the target if the control is $|1\rangle$.
- `Toffoli @i0 @i1 @i2` takes as input two *control* `Qubit`s with local metrics `i0` and `i1`, and a *target* `Qubit` with local metric `i2`. It negates the target if both controls are $|1\rangle$.

### Classically controlled gates
- `CCNot`: like `CNot`, but the control is a classical `Bit`.
- `CCZ`: like `CZ`, but the control is a classical `Bit`.

## Standard Library

**Note**: For simplicity, the types of the following library functions are annotated according to a width/depth analysis. Please note that annotations may change when analysing other metrics (e.g. gate count).

### Qubit and Bit meta-operations
- `qinit0 :: ![1]Qubit{0}` wrapper for `QInit0`.
- `qinit1 :: ![1]Qubit{0}` wrapper for `QInit1`.
- `qdiscard :: ![0](forall[0,0] l. Qubit{l} -o[1,0] ())` wrapper for `QDiscard`.
- `cinit0 :: ![1]Bit{0}` wrapper for `CInit0`.
- `cinit1 :: ![1]Bit{0}` wrapper for `CInit0`.
- `cdiscard :: ![0](forall[0,0] l. Bit{l} -o[1,0] ())` wrapper for `CDiscard`.
- `meas :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Bit{l+1})` wrapper for `Meas`.

### Single-qubit gates
- `hadamard :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` wrapper for `Hadamard`.
- `qnot :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` wrapper for `PauliX`.
- `pauliY :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` wrapper for `PauliY`.
- `pauliZ :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` wrapper for `PauliZ`.
- `tgate :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` wrapper for `T`.

### Controlled gates
- `cnot :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` wrapper around `CNot`
- `cz :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` wrapper around `CZ`
- `toffoli :: ![0](forall[0,0] lctrl1. forall[0,0] lctrl2. forall[0,0] ltrgt. Qubit{lctrl1} -o[1,0] Qubit{lctrl2} -o[2,1] Qubit{ltrgt} -o[3,2] (Qubit{max(lctrl1, lctrl2, ltrgt) + 1}, Qubit{max(lctrl1, lctrl2, ltrgt) + 1}, Qubit{max(lctrl1, lctrl2, ltrgt) + 1}))` wrapper around `Toffoli`
- `ccnot :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Bit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Bit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` wrapper around `CCNot`
- `ccz :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Bit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Bit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` wrapper around `CCZ`

### Parametric gates
- `rgate :: ![0](forall[0,0] n. forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` parametric rotation gate. Applies a rotation of 2π/2^n radians around the Z-axis to a single `Qubit` with local metric `l`.
- `cr :: ![0](forall[0,0] n. forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` controlled parametric rotation gate. Takes as input a *control* `Qubit` with local metric `lctrl` and a *target* `Qubit` with local metric `ltrgt` and applies a rotation of 2π/2^n radians around the Z-axis to the target if the control is $|1\rangle$. Note that this operation is symmetrical.
- `mcnot :: ![0](forall[0,0] n. forall[0,0] lctrls. forall[0,0] ltrgt. List[_<n] Qubit{lctrls} -o[n,0] Qubit{ltrgt} -o[n+1, n] (List[i<n] Qubit{max(lctrls, ltrgt) + 1}, Qubit{max(lctrls, ltrgt) + 1}))` multiply-controlled negation. Takes as input a list of n *control* `Qubit`s with local metric `lctrls` and a *target* `Qubit` with local metric `ltrgt`. Negates the target if all controls are $|1\rangle$. **Note:** although each gate in this family counts as a single elementary gate, a physical implementation is not guaranteed to exist. Take this into account when reading the results of the analysis, especially if you are interested in the circuit's gate count.

### Other
- `range :: forall[0,0] n. List[i<n] ()` returns a list containing exactly `n` unit values.