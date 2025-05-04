# Prelude Reference

For simplicity, the types of the following library functions are annotated according to a width global analysis and depth local analysis. Please note that type annotations will likely be different when analysing other metrics.


## Qubit handling

### `qinit0 :: ![1] Qubit{0}`
Initialize a qubit in the basis state 0.

### `qinit1 :: ![1] Qubit{0}`
Initialize a qubit in the basis state 1.

### `qdiscard :: ![0](forall[0,0] d. Qubit{d} -o[1,0] ())`

Discard a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                  |
|----------|-------------|------------------------------|
| `q`      | `Qubit{d}`  | The qubit to discard.        |

### `meas :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Bit{d+1})`

Measure a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                  |
|----------|-------------|------------------------------|
| `q`      | `Qubit{d}`  | The qubit to measure.        |

**Returns:** a bit at depth `d+1` containing the result of the measurement.

## Bit handling


### `cinit0 :: ![1] Bit{0}`
Initialize a bit in the 0 state.

### `cinit1 :: ![1] Bit{0}`
Initialize a bit in the 1 state.

### `cdiscard :: ![0](forall[0,0] d. Bit{d} -o[1,0] ())`

Discard a bit `b` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input bit. |

| Argument | Type        | Description                  |
|----------|-------------|------------------------------|
| `b`      | `Bit{d}`  | The bit to discard.        |


## Single-qubit gates

### `hadamard :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})`
Apply the Hadamard gate to a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                         |
|----------|-------------|-------------------------------------|
| `q`      | `Qubit{d}`  | The input qubit to Hadamard.        |

**Returns:** a qubit at depth `d+1`.


### `qnot :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})`
Apply the Pauli-X gate to a qubit `q` at depth `d`. This behaves like classical negation on the basis states.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                         |
|----------|-------------|-------------------------------------|
| `q`      | `Qubit{d}`  | The input qubit to Pauli-X.         |

**Returns:** a qubit at depth `d+1`.

### `pauliY :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})`
Apply the Pauli-Y gate to a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                         |
|----------|-------------|-------------------------------------|
| `q`      | `Qubit{d}`  | The input qubit to Pauli-Y.         |

**Returns:** a qubit at depth `d+1`.

### `pauliZ :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})`
Apply the Pauli-Z gate to a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                         |
|----------|-------------|-------------------------------------|
| `q`      | `Qubit{d}`  | The input qubit to Pauli-Z.         |

**Returns:** a qubit at depth `d+1`.

### `tgate :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})`
Apply the T gate to a qubit `q` at depth `d`.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `d`       | Maximum depth of the input qubit. |

| Argument | Type        | Description                            |
|----------|-------------|----------------------------------------|
| `q`      | `Qubit{d}`  | The input qubit to the T gate.         |

**Returns:** a qubit at depth `d+1`.


## Controlled gates

### `cnot :: ![0](forall[0,0] dctrl. forall[0,0] dtrgt. Qubit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Qubit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))`
Apply the controlled-not gate to a control qubit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control qubit. |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Qubit{dctrl}` | The control qubit.         |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control and target qubits, both at depth `max(dctrl, dtrgt) + 1`.

### `cz :: ![0](forall[0,0] dctrl. forall[0,0] dtrgt. Qubit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Qubit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))`
Apply the controlled-Z gate to a control qubit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control qubit. |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Qubit{dctrl}` | The control qubit.         |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control and target qubits, both at depth `max(dctrl, dtrgt) + 1`.

### `toffoli :: ![0](forall[0,0] dctrl1. forall[0,0] dctrl2. forall[0,0] dtrgt. Qubit{dctrl1} -o[1,0] Qubit{dctrl2} -o[2,1] Qubit{dtrgt} -o[3,2] (Qubit{max(dctrl1, dctrl2, dtrgt) + 1}, Qubit{max(dctrl1, dctrl2, dtrgt) + 1}, Qubit{max(dctrl1, dctrl2, dtrgt) + 1}))`
Apply the Toffoli gate to two control qubits `ctrl1` and `ctrl2` at depth `dctrl1` and `dctrl2`, respectively, and a target qubit `trgt` at depth `dtrgt`.

| Parameter | Description                                |
|-----------|--------------------------------------------|
| `dctrl1`  | Maximum depth of the first control qubit.  |
| `dctrl2`  | Maximum depth of the second control qubit. |
| `dtrgt`   | Maximum depth of the target qubit.         |

| Argument | Type            | Description               |
|----------|-----------------|---------------------------|
| `ctrl1`  | `Qubit{dctrl1}` | The first control qubit.  |
| `ctrl2`  | `Qubit{dctrl2}` | The second control qubit. |
| `trgt`   | `Qubit{dtrgt}`  | The target qubit.         |

**Returns:** the two controls and the target qubit, all at depth `max(dctrl1, dctrl2, dtrgt) + 1`.

### `ccnot :: ![0](forall[0,0] dctrl. forall[0,0] dtrgt. Bit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Bit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))`
Apply the classically-controlled not gate to a control bit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control bit.   |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Bit{dctrl}`   | The control bit.           |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control bit and the target qubit, both at depth `max(dctrl, dtrgt) + 1`.

### `ccz :: ![0](forall[0,0] dctrl. forall[0,0] dtrgt. Bit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Bit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))`
Apply the classically-controlled Z gate to a control bit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control bit.   |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Bit{dctrl}`   | The control bit.           |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control bit and the target qubit, both at depth `max(dctrl, dtrgt) + 1`.


## Parametric gates

### `rgate :: ![0](forall[0,0] n. forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})` 
Parametric rotation gate. Apply a rotation of `2π/2^n` radians around the Z-axis to a qubit `q` at depth `d`.

| Parameter | Description                            |
|-----------|----------------------------------------|
| `n`       | Determines the angle of the rotation.  |
| `d`       | Maximum depth of the target qubit.     |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `q`      | `Qubit{d}`     | The qubit to rotate.       |

**Returns:** a qubit at depth `d+1`.

### `invrgate :: ![0](forall[0,0] n. forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})` 
Parametric rotation gate. Inverse of `rgate`. Apply a rotation of `-2π/2^n` radians around the Z-axis to a qubit `q` at depth `d`.

| Parameter | Description                            |
|-----------|----------------------------------------|
| `n`       | Determines the angle of the rotation.  |
| `d`       | Maximum depth of the target qubit.     |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `q`      | `Qubit{d}`     | The qubit to rotate.       |

**Returns:** a qubit at depth `d+1`.

### `cr :: ![0](forall[0,0] n. forall[0,0] dctrl. forall[0,0] dtrgt. Qubit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Qubit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))` 
Parametric controlled rotation gate. Apply a rotation of `2π/2^n` radians around the Z-axis to a control qubit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`. Note that this gate is symmetrical.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control qubit. |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Qubit{dctrl}` | The control qubit.         |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control bit and the target qubit, both at depth `max(dctrl, dtrgt) + 1`.

### `invcr :: ![0](forall[0,0] n. forall[0,0] dctrl. forall[0,0] dtrgt. Qubit{dctrl} -o[1,0] Qubit{dtrgt} -o[2,1] (Qubit{max(dctrl, dtrgt) + 1}, Qubit{max(dctrl, dtrgt) + 1}))` 
Parametric controlled rotation gate. Inverse of `cr`. Apply a rotation of `-2π/2^n` radians around the Z-axis to a control qubit `ctrl` at depth `dctrl` and a target qubit `trgt` at depth `dtrgt`. Note that this gate is symmetrical.

| Parameter | Description                         |
|-----------|-------------------------------------|
| `dctrl`   | Maximum depth of the control qubit. |
| `dtrgt`   | Maximum depth of the target qubit.  |

| Argument | Type           | Description                |
|----------|----------------|----------------------------|
| `ctrl`   | `Qubit{dctrl}` | The control qubit.         |
| `trgt`   | `Qubit{dtrgt}` | The target qubit.          |

**Returns:** the control bit and the target qubit, both at depth `max(dctrl, dtrgt) + 1`.

### `mcnot :: ![0](forall[0,0] n. forall[0,0] dctrls. forall[0,0] dtrgt. List[i<n] Qubit{dctrls} -o[n,0] Qubit{dtrgt} -o[n+1, n] (List[i<n] Qubit{max(dctrls, dtrgt) + 1}, Qubit{max(dctrls, dtrgt) + 1}))` 
Multi-controlled not gate. Apply a controlled-not operation to a list `ctrls` of `n` control qubits at depth `dctrls` and a target qubit `trgt` at depth `dtrgt`.

**Note:** although each gate in this family counts as a single elementary gate, a physical implementation is not guaranteed to exist. Take this into account when reading the results of the tool, especially during gate count analysis.

| Parameter | Description                          |
|-----------|--------------------------------------|
| `n`       | Number of control qubits.            |
| `dctrls`  | Maximum depth of the control qubits. |
| `dtrgt`   | Maximum depth of the target qubit.   |

| Argument | Type                       | Description                  |
|----------|----------------------------|------------------------------|
| `ctrls`  | `List[i<n] Qubit{dctrls}`  | The list of control qubits.  |
| `trgt`   | `Qubit{dtrgt}`             | The target qubit.            |

## Other

### `range :: ![0](forall[0,0] n. List[i<n] ())`
Generate a list of unit values for iteration purposes.

| Parameter | Description                       |
|-----------|-----------------------------------|
| `n`       | The desired length of the list.   |

**Returns:** a list containing exactly `n` unit values.