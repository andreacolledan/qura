# Prelude Reference

**Note**: For simplicity, the types of the following library functions are annotated according to a width/depth analysis. Please note that annotations may change when analysing other metrics (e.g. gate count).

## Qubit and Bit meta-operations
- `qinit0 :: ![1]Qubit{0}` initialize a qubit in the `|0>` state.
- `qinit1 :: ![1]Qubit{0}` initialize a qubit in the `|1>` state.
- `qdiscard :: ![0](forall[0,0] l. Qubit{l} -o[1,0] ())` discard a qubit at depth `l`.
- `cinit0 :: ![1]Bit{0}` initialize a bit in the `0` state.
- `cinit1 :: ![1]Bit{0}` initialize a bit in the `1` state.
- `cdiscard :: ![0](forall[0,0] l. Bit{l} -o[1,0] ())` discard a bit at depth `l`.
- `meas :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Bit{l+1})` measure a qubit at depth `l`.

## Single-qubit gates
- `hadamard :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` apply the Hadamard gate to a qubit at depth `l`.
- `qnot :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` apply the Pauli-X (negation) gate to a qubit at depth `l`.
- `pauliY :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` apply the Pauli-Y gate to a qubit at depth `l`.
- `pauliZ :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` apply the Pauli-Z gate to a qubit at depth `l`.
- `tgate :: ![0](forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` apply the T gate to a qubit at depth `l`.

## Controlled gates
- `cnot :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` apply the controlled-not gate to a control qubit at depth `lctrl` and a target qubit at depth `ltrgt`.
- `cz :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` apply the controlled-Z gate to a control qubit at depth `lctrl` and a target qubit at depth `ltrgt`.
- `toffoli :: ![0](forall[0,0] lctrl1. forall[0,0] lctrl2. forall[0,0] ltrgt. Qubit{lctrl1} -o[1,0] Qubit{lctrl2} -o[2,1] Qubit{ltrgt} -o[3,2] (Qubit{max(lctrl1, lctrl2, ltrgt) + 1}, Qubit{max(lctrl1, lctrl2, ltrgt) + 1}, Qubit{max(lctrl1, lctrl2, ltrgt) + 1}))` apply the Toffoli gate to two control qubits at depth `lctrl1` and `lctrl2`, and a target qubit at depth `ltrgt`.
- `ccnot :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Bit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Bit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` apply the classically-controlled not gate to a control bit at depth `lctrl` and a target qubit at depth `ltrgt`.
- `ccz :: ![0](forall[0,0] lctrl. forall[0,0] ltrgt. Bit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Bit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` apply the classically-controlled Z gate to a control bit at depth `lctrl` and a target qubit at depth `ltrgt`.

## Parametric gates
- `rgate :: ![0](forall[0,0] n. forall[0,0] l. Qubit{l} -o[1,0] Qubit{l+1})` parametric rotation gate. Apply a rotation of `2π/2^n` radians around the Z-axis to a qubit at depth `l`.
- `cr :: ![0](forall[0,0] n. forall[0,0] lctrl. forall[0,0] ltrgt. Qubit{lctrl} -o[1,0] Qubit{ltrgt} -o[2,1] (Qubit{max(lctrl, ltrgt) + 1}, Qubit{max(lctrl, ltrgt) + 1}))` parametric controlled rotation gate. Apply a rotation of `2π/2^n` radians around the Z-axis to a control qubit at depth `lctrl` and a target qubit at depth `ltrgt`. Note that this gate is symmetrical.
- `mcnot :: ![0](forall[0,0] n. forall[0,0] lctrls. forall[0,0] ltrgt. List[_<n] Qubit{lctrls} -o[n,0] Qubit{ltrgt} -o[n+1, n] (List[i<n] Qubit{max(lctrls, ltrgt) + 1}, Qubit{max(lctrls, ltrgt) + 1}))` multi-controlled not gate. Apply a controlled-not operation to a list of `n` control qubits at depth `lctrls` and a target qubit at depth `ltrgt`. **Note:** although each gate in this family counts as a single elementary gate, a physical implementation is not guaranteed to exist. Take this into account when reading the results of the analysis, especially if you are interested in the circuit's gate count.

## Other
- `range :: forall[0,0] n. List[i<n] ()` return a list containing exactly `n` unit values.