## Proto-Quipper-R Documentation (WIP)

QuRA takes as input programs written in a variant of Quipper called Proto-Quipper-R. This language is based on the Proto-Quipper family of theoretical programming languages. As such, Proto-Quipper-R is, at its core, a lambda calculus with bespoke constructs to describe and manipulate quantum circuits, dressed in a Haskell-like syntax.

## Table of contents

- [Proto-Quipper-R Documentation (WIP)](#proto-quipper-r-documentation-wip)
- [Table of contents](#table-of-contents)
- [Basics](#basics)
  - [Bits, qubits and circuits](#bits-qubits-and-circuits)
  - [Linear functions](#linear-functions)
  - [Dependent functions](#dependent-functions)
    - [Abstract resource indices](#abstract-resource-indices)
  - [Defining custom circuit operations](#defining-custom-circuit-operations)
  - [Recursion](#recursion)
- [Type Checking and Width Verification](#type-checking-and-width-verification)
- [Primitive Operations](#primitive-operations)
  - [Qubit and Bit meta-operations](#qubit-and-bit-meta-operations)
  - [Single-qubit gates](#single-qubit-gates)
  - [Controlled gates](#controlled-gates)
  - [Classically controlled gates](#classically-controlled-gates)
  - [Convenience gates](#convenience-gates)
  - [Other primitives](#other-primitives)

## Basics

### Bits, qubits and circuits
The fundamental objects of circuit-building are `Bit`s and `Qubit`s. Because Proto-Quipper-R is a *circuit description language*, values of these types do not store the state of a bit or qubit. Rather, they are references to available wires in a global underlying circuit that is built incrementally as the program executes. You can think of them as the quantum circuit counterpart to pointers to memory locations.

Unlike memory locations, however, bits and qubits have to obey additional constraints: due to the no-cloning property of quantum states, thet cannot be duplicated, and due to decoherence they cannot go out of scope either. Whenever a new bit or qubit is initialized, it must be used *exactly once*, that is to say, bit and qubit references obey a linear typing discipline.

All operations that actively modify the underlying circuit are carried out through the `apply` operator. This inlcudes bit and qubit initialization. `apply` takes as arguments a circuit operation `op` and a tuple or list of wire references `wr`, whose wires are fed into the operation. An operation with no inputs is fed the unit value `()` instead. `apply` then emits said operation to the underlying circuit and returns a new collection of wires, corresponding to the outputs of `op`.

For example, we can initialize a qubit, apply some gates to it and discard it as follows:
```
let q = apply(QInit0, ()) in
let q = apply(Hadamard, q) in
apply(QDiscard, q)
```
where `QInit0`, `Hadamard` and `QDiscard` are primitive circuit operations in Proto-Quipper-R. You can find a list of all primitive circuit operations at the end of this page.
For convenience, QuRA's standard library comes with a number of functions that wrap the application of common quantum operations in a nicer interface. For example, the previous piece of code can be rewritten as follows:
```
let q = force qinit0 () in
let q = force hadamard q in
force discard q
```
where `force` is used to unwrap a duplicable term. More on this in the following section.

### Linear functions
Functions in Proto-Quipper-R are defined through lambdas. The argument of the lambda must be annotated with its type. For example, `swap` is a linear function that swaps two qubit references:

```
let swap = \(x,y) :: (Qubit, Qubit) . (y,x)
in swap (force qinit1 (), force qinit0 ())
```
Assuming we are keeping track of width, the type of `swap` is `(Qubit, Qubit) -o[2,0] (Qubit, Qubit)`, meaning that it is a linear function that takes a pair of qubits as input, builds a circuit of width `2` (since those qubits have an intrinsic width) and returns a pair of qubits.

This function is treated linearly, meaning that once we apply `swap` to an argument, we can no longer use it. This might seem counterintuitive, but there is good reason for that. The `0` annotation in the `swap`'s signature tells us that this function does not capture wires from the surrounding scope. However, in general a function may enclose wires or other linear variables in its closure. Consider, for example, the following function

```
let q = force qinit0 () in
let capture = \p :: Qubit . force cnot q p
```

This is a valid Proto-Quipper-R function. Until we apply `capture` to a suitable argument, however, qubit `q` is not consumed. Rather, it lives "suspended" in `capture`'s closure. Duplicating `capture` (e.g. by applying it multiple times) would therefore amount to duplicating qubit `q`, which is impossible due to the no-cloning theorem. For this reason, functions are treated linearly by default.

This might sound undesirable: in most cases, we define pure functions with the express intention of reusing the same code multiple times, and `swap` is one such case. Luckily, there is a way out: a function that contains no linear resources from outside its scope can be made duplicable by wrapping it in the `lift` construct. A lifted expression can then be unwrapped via the `force` construct that we saw in the previous examples. Because `swap` from the previous example does not contain linear resources, we can make it duplicable as follows:
```
let swap = lift \(x,y) :: (Qubit, Qubit) . (y,x)
in force swap $ force swap (force qinit1 (), force qinit0 ())
```
Now `swap` has type `![0]((Qubit, Qubit) -o[2,0] (Qubit, Qubit))`. The bang type constructor `!` signifies that we are now talking about a *duplicable* function with the same characteristics as before. It is annotated with the size of the circuit produced once we `force` the corresponding expression (in this case, we produce no circuit).

### Dependent functions

The other kind of functions available in Proto-Quipper-R are dependent functions. Dependency is very limited in the language: dependent functions may only take natural numbers (i.e. indices) as arguments. For example, the following function takes as input an index `i`, a single-qubit operation `op` of width at most `i+1`, a qubit `q` and applies `op` to `q` twice:

```
let twice = @i . \(c, q) :: (Circ[i+1](Qubit,Qubit), Qubit) .
  let q = apply(c,q)
  in apply(c,q)
in twice @1 (Hadamard, (force qinit0 ()))
```
The `twice` function has type `i ->[0, 0] (Circ [(i + 1)] (Qubit, Qubit), Qubit) -o[i+1, 0] Qubit`. This is slightly different notation from other dependently typed languages: because natural indices are the only terms that types are allowed to depend on, we need not specify the type of `i`. At the present moment, the language of indices has the following grammar:
```
I,J ::= n | i | I + J | I - J | I * J | max(I,J) | max[i<I] J | sum[i<I] J,
```
where `n` is a natural number, `i` is an index variable, `I - J` is *natural* subtraction (such that `I - J` is 0 whenever `I` < `J`), and `max[i<I] J` and `sum[i<I] J` are respectively the maximum and the sum for `i` going from 0 (included) to `I` (excluded) of `J` (where `i` may appear in `J`).

#### Abstract resource indices

TODO

### Defining custom circuit operations
TODO box

### Recursion
TODO fold

## Type Checking and Width Verification

TODO

## Primitive Operations

At the time of writing, the following are the primitive operations that can be used for circuit-building in Proto-Quipper-R

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