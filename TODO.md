- [X] Add top-level definitions
  - [X] Add primitive modules (i.e. lists of top-level definitions)
  - [X] Add top-level definitions without arguments
  - [X] Add top-level definitions with arguments
  - [X] Add top-level definitions with patterns
  - [X] Change tool interface to only consider top-level definitions
  - [X] Update README
- [X] Major code refactoring
- [ ] Add 'undefined' for experimentation purposes
- [ ] Better syntax and handling of independent list types
- [ ] Add proper documentation for the PQ language
- [ ] Add actual module support with linking
- [ ] Fix parsing of indices embedded in types and expressions
- [ ] Make closure annotations transparent to the user
- [ ] Better error messages for type inference
  - [ ] Add line numbers
  - [ ] Add original source text instead of printing the AST

### Interpreter

* [ ] clean the code
* [ ] adjust imports/exports lists of the new haskel modules

  * [X] used copilot to tighten the imports. instead of importingt he whole modules we only import the needed functions
* [X] qubit reuse option (init new qubits on prev. discarded qubits))
* [ ] check `isub::IndexSubstitution->Expr->Expr`
* [ ] maybe a prettier way for the rotations angle isntead of writing a ginormous fraction
* [X] qasm getsimple does not account for using the leats deep wires for recycling... check getsimple todo
* [X] non so che succede se un let definisce una funzione già esistente
* [ ] fix applymdoules to the abs
* [X] consider removing test/pos/12.pq as MakeMCNot no longer exists -- REMOVED
* [X] add interpreter tests to `stack test`
