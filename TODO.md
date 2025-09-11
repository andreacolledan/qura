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
* [X] qubit reuse option (init new qubits on prev. discarded qubits))
* [ ] check `fresh` :)
* [ ] in qiskit you cant initialize bits (?!?!??!), maybe add a --qiskit cla
* [ ] i cant find other backends beside qiskit, but it is limited
* [X] for now we execute the last definition if it is the main. we could maybe avoid wanting that it is the last possible definition (even if we will have an error if it uses functions defined later, so it wil most of the times be the last one) but still)
* [ ] README note that to compute the depth of classically controlled gates in qasm, we use qubits isnstead of bits, so idk maybe something could change slightly
* [ ] **check all the CHECKME !!!!**
* [X] maybe to compute the depth we defer to qiskit depth... or in any case we analyze the qasm maybe using qiskit and we only keep metrics of the simplified circuit to double check
* [ ] initializations arent accounted in qura. maybe we dont do that either... (the problem is that in qasm to init1 you have to apply an x, that is a gate ofc)
* [ ] maybe a prettier way for the rotations angle isntead of writing a ginormous fraction
