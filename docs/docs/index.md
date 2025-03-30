# QuRA

QuRA is a tool for the static resource analysis of quantum algorithms described in PQ, a strongly typed circuit description language.

PQ features a rich type-and-effect system, which allows programmers to include quantitative information regarding the resource consumption of a program in its type. If a program type-checks in QuRA, then it's guaranteed to not exceed the specified resource requirements.

- [Getting started with QuRA](tool/getting-started.md)
- [PQ documentation](language/overview.md)
- [How to extend QuRA](tool/extensions.md)
