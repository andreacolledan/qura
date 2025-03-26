# QuRA

QuRA is a static analysis tool for the resource consumption of quantum algorithms described in PQ.

PQ is a quantum circuit description language that features a rich type-and-effect system, which allows programmers to include quantitative information regarding the resource requirements of a program in its type. Then, if the program type-checks in QuRA, we know for certain that it will not consume more resources than specified.

- [Getting started with QuRA](tool/tutorial.md)
- [PQ documentation](language/index.md)
- [How to extend QuRA](tool/extension.md)
