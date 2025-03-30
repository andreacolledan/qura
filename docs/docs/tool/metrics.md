# Circuit size metrics

## Common metrics

### Width

TODO

### Gate count

TODO

### Depth

TODO

## List of available metrics

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