# Roadmap

## Language

- [X] Add top-level definitions
  - [X] Add primitive modules (i.e. lists of top-level definitions)
  - [X] Add top-level definitions without arguments
  - [X] Add top-level definitions with arguments
  - [X] Add top-level definitions with patterns
  - [X] Change tool interface to only consider top-level definitions
  - [X] Update README
- [ ] Better syntax
  - [ ] Simpler syntax for (and handling of) independent list types
  - [X] Syntactic sugar for null closure annotations
- [ ] Better parsing of indices embedded in types and expressions
- [ ] Implement 'undefined' for experimentation purposes
- [ ] Language pragmas to enforce verification of certain resources

## Type checking

- [ ] Better error messages for type inference
  - [ ] Line numbers and original source in AST
  - [ ] Better description of errors
- [ ] Make closure annotations transparent to the user
- [ ] Better error messages for type inference
  - [ ] Add line numbers
  - [ ] Add original source text instead of printing the AST
- [ ] Bugfixes:
  - [ ] When checking list subtypes, element type subtyping should be checked under the assumption that the list counter is strictly less than the list length (currently, it is only assumed that it is no greater than it)

## Interpreter

- [ ] clean the code
- [X] adjust imports/exports lists of the new haskel modules
  - [X] used copilot to tighten the imports. instead of importingt he whole modules we only import the needed functions
- [X] qubit reuse option (init new qubits on prev. discarded qubits)
- [ ] check `isub :: IndexSubstitution -> Expr -> Expr`
- [ ] maybe a prettier way for the rotations angle instead of writing a huge fraction
- [X] qasm getsimple does not account for using the leats deep wires for recycling... check getsimple todo
- [ ] fix applymdoules to the abs
- [X] remove test/pos/12.pq as MakeMCNot no longer exists
- [X] add interpreter tests to `stack test`
- [ ] remove uses of `trace`

## Other

- [ ] Proper documentation for the PQ language
- [ ] Support for multi-module projects and linking


