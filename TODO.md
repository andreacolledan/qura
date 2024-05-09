# TODO

## Design

### Circuits

 - [x] Decide how to represent circuits, circuits with input and circuit-building computations. (Done: as in the paper)
 - [x] Decide how programs are meant to be executed: from a starting circuit or standalone. Consequently, choose whether $Q$ should appear in the implementation of the language type system or not. (Done: the former)

## Implementation

### Wire language checking

- [x] Label pairs
  - [x] Implementing
  - [x] Testing
- [x] Implement sized lists of labels
  - [x] Implementing
  - [x] Testing

### PQR Language checking

- [x] Language constructs
  - [x] Boxed circuits and apply
    - [x] Implementing boxed circuits
    - [x] Testing boxed circuits
    - [x] Implementing apply
    - [x] Testing apply
  - [x] Tuples and destructuring let
    - [x] Implementing tuples
    - [x] Testing tuples
    - [x] Implementing destructuring let
    - [x] Testing destructuring let
  - [x] Abstraction and application
    - [x] Implementing abstraction
    - [x] Testing abstraction
    - [x] Implementing application
    - [x] Testing application
  - [x] Lifting and forcing (aka linear boxing and unboxing of terms)
    - [x] Implementing lift
    - [x] Testing lift
    - [x] Implementing force
    - [x] Testing force
  - [x] Return and sequencing let
    - [x] Implementing return
    - [x] Testing return
    - [x] Implementing sequencing let
    - [x] Testing sequencing let
  - [x] Circuit boxing
    - [x] Implementing box
    - [x] Testing box
  - [x] Lists and fold
    - [x] Implementing lists
    - [x] Testing lists
    - [x] Implementing fold
    - [x] Testing fold
- [x] Well-formedness constraints
- [x] Subsumption (e.g. `⊢ m : Circ 1 (Qubit,Qubit) ; 5` entails `⊢ m : Circ 2 (Qubit,Qubit) ; 8`)
  - [x] Decide whether to implement subsumption as a distinct case or within rules. Decided the former.
  - [x] Implement subsumption
    - [x] Implement subtyping
      - [x] Interface with SMT solver
- [x] Instantiation (e.g. `⊢ f : List i Qubit -o [i,0] List i Qubit` entails `⊢ f : List 3 Qubit -o [3,0] List 3 Qubit`)
  - [x] Implement instantiation

- [ ] Unified syntax
  - [x] AST
  - [x] Type inference
  - [ ] Testing (WIP)

- [x] Pattern matching

- [x] Better error messages
- [x] Better handling of shadowing
- [x] Better unification
  
### Parser

- [x] Parsing wire bundles
- [x] Parsing indices
- [x] Parsing PQR
- [ ] Switch to `megaparsec`
- [ ] Better parser (e.g. solve postfix operators always requiring parentheses) (WIP)
- [ ] Top-level definitions

### Performance

- [x] SMT solving with one file per program (as opposed to per query)
- [x] Use HashMap in place of Map

### Examples

- [x] List functions
- [x] QFT
- [x] Grover
- [x] Arithmetic (adder)

