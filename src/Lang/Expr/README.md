## Proto-Quipper-R Documentation (WIP)

QuRA takes as input programs written in a variant of Quipper called Proto-Quipper-R. This language is based on the Proto-Quipper family of theoretical programming languages. As such, Proto-Quipper-R is, at its core, a lambda calculus with bespoke constructs to describe and manipulate quantum circuits.

## Basics

### Bits, Qubits and Circuits
The fundamental objects of circuit-building are `Bit`s and `Qubit`s. Because Proto-Quipper-R is a *circuit description language*, values of these types do not store the state of a bit or qubit. Rather, they are references to available wires in a global underlying circuit that is built incrementally as the program executes. You can think of them as the quantum circuit counterpart to pointers to memory locations.

Unlike memory locations, however, bits and qubits have to obey additional constraints: due to the no-cloning property of quantum states, thet cannot be duplicated, and due to decoherence they cannot go out of scope either. Whenever a new bit or qubit is initialized, it must be used *exactly once*, that is to say, bit and qubit references obey a linear typing discipline.

All operations that have an impact on the circuit being built must be carried out through the `apply` operator. This inlcude bit and qubit initialization. `apply` takes as arguments a circuit operation `op` and a collection of wire references `wr` to be fed to that operation (or `()` if `op` takes no input). It then emits said operation to the underlying circuit and returns a new collection of wires, corresponding to the outputs of `op`.

For example, we can initialize a qubit, apply some gates to it and discard it as follows:
```
let q = apply(QInit0, ()) in
let q = apply(Hadamard, q) in
apply(QDiscard, q)
```
where `QInit0`, `Hadamard` and `QDiscard` are primitive circuit operations in Proto-Quipper-R. You can find a list of all primitive circuit operations can be found at the end of this page.
For convenience, QuRA's standard library comes with a number of functions that wrap the application of common quantum operations in a nicer interface. For example, the previous piece of code can be rewritten as follows:
```
let q = force qinit0 () in
let q = force hadamard q in
force discard q
```
where `force` is used to unwrap a duplicable term. More on this in the following section.

### Linear Functions
Functions in Proto-Quipper-R are defined through lambdas. The argument of the lambda must be annotated with its type. For example, `swap` is a linear function that swaps two qubit references:

```
let swap = \(x,y) :: (Qubit, Qubit) . (y,x)
in swap (force qinit1 (), force qinit0 ())
```
The type of `swap` is `(Qubit, Qubit) -o[2,0] (Qubit, Qubit)`, meaning that it is a linear function that takes a pair of qubits as input, builds a circuit of width `2` (since those qubits have an intrinsic width) and returns a pair of qubits. The `0` index tells us that this function does not capture wires from the surrounding scope. Precisely because functions may in principle contain wires or other linear resources, they are treated by default as linear resources themselves, that is, they must be applied exactly once. This is undesirable in many cases, so a function that contains no linear resources from outside its scope can be made duplicable through the `lift` construct. A lifted expression can then be recovered via the `force` construct. Because `swap` from the previous example does not contain linear resources, we can make it duplicable as follows:
```
let swap = lift \(x,y) :: (Qubit, Qubit) . (y,x)
in force swap $ force swap (force qinit1 (), force qinit0 ())
```
Now `swap` has type `!((Qubit, Qubit) -o[2,0] (Qubit, Qubit))` meaning that it is a *duplicable* function with the same characteristics as before.

### Dependent Functions

The other kind of functions available in Proto-Quipper-R are dependent functions. Dependency is very limited in the language: dependent functions may only take natural numbers (i.e. indices) as arguments. For example, the following function takes as input an index `i`, a single-qubit operation `op` of width at most `i+1`, a qubit `q` and applies `op` to `q` twice:

```
let twice = @i . \(c, q) :: (Circ[i+1](Qubit,Qubit), Qubit) .
  let q = apply(c,q)
  in apply(c,q)
in twice @1 (Hadamard, (force qinit0 ()))
```
The `twice` function has type `i ->[0, 0] (Circ [(i + 1)] (Qubit, Qubit), Qubit) -o[i+1, 0] Qubit`. This is slightly different notation from other dependently typed languages: because natural indices are the only terms that types are allowed to depend on, we need not specify the type of `i`. At the present moment, the language of indices has the following grammar:
```
I,J ::= n | i | I + J | I - J | I * J | max(I,J) | max[i<I] J,
```
where `n` is a natural number, `i` is an index variable, `I - J` is *natural* subtraction (such that `I - J` is 0 whenever `I` < `J`) and `max[i<I] J` is the maximum for `i` going from 0 (included) to `I` (excluded) of `J` (where `i` may appear in `J`).

### Custom Circuit Operations
TODO box

### Recursion
TODO fold

## Type Checking and Width Verification

TODO

## Primitive Operations

At the time of writing, the following are the primitive operations that can be used for circuit-building in Proto-Quipper-R

### Qubit and Bit meta-operations

- `QInit0` and `QInit1` initialize a new `Qubit` to the states $|0\rangle$ and $|1\rangle$, respectively.
- `QDiscard` discards a `Qubit`.
- `CInit0` and `CInit1` initialize a new `Bit` to the states $0$ and $1$, respectively.
- `CDiscard` discards a `Bit`.
- `Meas` measures a `Qubit`, returning a `Bit`.
  
### Single-qubit gates

- `Hadamard` performs the Hadamard transform on a single `Qubit`.
- `PauliX` flips a single `Qubit` over the x-axis, i.e. it negates its input.
- `PauliY` flips a single `Qubit` over the y-axis.
- `PauliZ` flips a single `Qubit` over the z-axis.

### Multi-qubit gates

- `CNot` takes as input a pair of `Qubit`s and negates the second (the *target*) if the first (the *control*) is $|1\rangle$. The control qubit is returned unchanged.
- `Toffoli` takes as input three `Qubit`s and negates the third (the *target*) if the first two (the *controls*) are $|1\rangle$. The control qubits are returned unchanged.
- `MakeNToffoli @n`, where `n` is a natural number, takes as input a list of `n` control `Qubit`s and a target `Qubit` and applies the `n`-controlled Toffoli gate to them.
- `MakeNCZ @n`, where `n` is a natural number, takes as input a list of `n` control `Qubit`s and a target `Qubit` and applies the `n`-controlled Pauli-Z gate to them.
- `MakeCRGate @i`, where `i` is a natural number, takes as input a pair of `Qubit`s and shifts the phase of the second (the *target*) by $2π/2^i$ radians if the first (the *control*) is $|1\rangle$. The control qubit is returned unchanged.

### Other primitives
- `MakeUnitList @n`, where `n` is a natural number, retuns a list of `()` of length `n`. Useful to simulate iteration using fold.