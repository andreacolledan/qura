![QuRA-Logo](Header.png)

[![CI](https://github.com/andreacolledan/qura/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/andreacolledan/qura/actions/workflows/build.yml)

QuRA is a static analysis tool for the resource consumption of quantum algorithms described in the PQ programming language.

PQ is a quantum circuit description language that features a rich type-and-effect system, which allows programmers to include quantitative information regarding the resource requirements of a program in its type. If a PQ program type-checks in QuRA, it is guaranteed to not consume more resources than specified.

[Official documentation](https://qura.readthedocs.io/en/latest/)


## Installing
**Note:** QuRA requires [cvc5](https://cvc5.github.io) to be installed and present in your `PATH`.

You can build and install QuRA using [stack](https://docs.haskellstack.org/en/stable/) by running

```
git clone https://github.com/andreacolledan/qura
cd qura
stack install
```

### VSCode Support

Syntax highlighting for PQ is available as a [VSCode extension](https://github.com/andreacolledan/vscode-pq-syntax-highlighting).

## Usage
To analyze program `file.pq`, simply run
```
qura file.pq
```
This runs standard type inference for `file.pq`, without any resource analysis. In order to perform global resource metric estimation, use the `-g METRIC` option. For example, to perform width estimation, run

```
qura file.pq -g width
```
To perform local metric estimation, use the `-l METRIC` option instead. Note that at most one global metric and on local metric can be analyzed at a time.

Some example programs are available in the `examples` directory. For instance, you can verify the width and depth of the quantum Fourier transform algorithm by running the following in the QuRA's root directory:

```
qura examples/qft.pq -g width -l depth
```

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

If you are interested in extending QuRA with new kinds of metric analysis, consult [this guide](https://qura.readthedocs.io/en/latest/tool/extensions/).
