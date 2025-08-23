# Roadmap

## Version 1.0.0

- [x] Add top-level definitions
  - [x] Add primitive modules (i.e. lists of top-level definitions)
  - [x] Add top-level definitions without arguments
  - [x] Add top-level definitions with arguments
  - [x] Add top-level definitions with patterns
  - [x] Change tool interface to only consider top-level definitions
  - [x] Update README
- [ ] Language pragmas to enforce verification of certain resources
  - [x] Implement pragma data and parsing
  - [ ] Add pragmas to main pipeline
- [ ] Better syntax
  - [ ] Simpler syntax for (and handling of) independent list types
  - [x] Syntactic sugar for null closure annotations
- [ ] Proper documentation for the PQ language
- [ ] Support for multi-module projects
- [ ] Better error messages for type inference
  - [ ] Line numbers and original source in AST
  - [ ] Better description of errors
- [ ] Better parsing of indices embedded in types and expressions
- [ ] Keyword 'undefined' for experimentation purposes
- [ ] Bugfixes:
  - [ ] When checking list subtypes, element type subtyping should be checked under the assumption that the list counter is strictly less than the list length (currently, it is only assumed that it is no greater than it)

## Future versions

- [ ] Interpreter