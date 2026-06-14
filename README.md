![QuRA-Logo](res/Header.png)

[![CI](https://github.com/andreacolledan/qura/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/andreacolledan/qura/actions/workflows/build.yml)

QuRA is a toolchain for the PQ quantum programming language. It provides:

- Type checking, also enforcing no-cloning
- Static analysis of resource consumption (width, depth, gate count, etc.)
- Interpretation of PQ programs, executing them and producing circuits runnable on a quantum backend


PQ is a quantum circuit description language with a rich type-and-effect system. Programs in PQ can include quantitative information about resource requirements (e.g., qubits, gates, depth) directly in their types. QuRA guarantees that if a PQ program type-checks, it will not consume more resources than specified.

[Official documentation (WIP)](https://qura.readthedocs.io/en/latest/)

## Download

The latest version of QuRA is available on GitHub at https://github.com/andreacolledan/qura.

Precompiled binaries for releases can be found [here](https://github.com/andreacolledan/qura/releases/).

## Installing
**Note:** QuRA requires [cvc5](https://cvc5.github.io) to be installed and available in your `PATH`.

You can build and install QuRA using [stack](https://docs.haskellstack.org/en/stable/) by running

```
git clone https://github.com/andreacolledan/qura
cd qura
stack install
```

### VSCode Support

Syntax highlighting for PQ is available as a [VSCode extension](https://github.com/andreacolledan/vscode-pq-syntax-highlighting).

## Usage
To run program `file.pq`:
```
qura file.pq
```
This runs simple type checking for `file.pq`, without any resource analysis, and emits the resulting QASM circuit to the standard output.

In order to verify the *global* resource requirements of the program, run the previous command with the additional `-g METRIC` option. For example, to verify the width of `file.pq` before running it:

```
qura file.pq -g width
```
To verify the *local* resource requirements of the program, add the `-l METRIC` option instead. Global resources include circuit metrics such as width and gate count, while local resources include various notions of circuit depth. For more information on the distinction between global and local resource metrics, refer to [the documentation](https://qura.readthedocs.io/en/latest/tool/getting-started/).

Note that at most one global resource metric and one local resource metric can be verified at a time.

### Try it out

The `examples` directory includes some ready-to-run PQ programs. For example, verify the width and depth requirements of the [quantum Fourier transform](https://en.wikipedia.org/wiki/Quantum_Fourier_transform) algorithm:

```
qura examples/qft.pq -g width -l depth -o qft.qasm
```

In the end, `qft.qasm` will contain a resource-verified QASM specification of the quantum Fourier transform circuit (of input size 4).

### Available resource metrics
Currently, QuRA supports the analysis of the following circuit size metrics:

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

For more general usage information, refer to `qura --help`.

## Contributing

We welcome contributions! Whether you want to:
- Add new metric analyses (see [this guide](https://qura.readthedocs.io/en/latest/tool/extensions/))
- Report bugs or suggest improvements
- Collaborate on the project

feel free to [open an issue](https://github.com/andreacolledan/qura/issues) or reach out to the maintainers.
