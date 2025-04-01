# The `fold` primitive

PQ does not support full-fledged recursion. However, you can iterate over lists using a built-in `fold` operator:

```hs
fold f acc list
```
folds a duplicable step function `f` over `list`, starting with accumulator `acc`. The step function is supposed to be polymorphic in an index variable that represents the iteration of the fold. If the accumulator's type depends on this variable, the step function must increase it by exactly 1 at each iteration.

More techically, let `{expr/i}` denote the substitution of arithmetic expression `expr` for index variable `i`. Whenever

- `list` has type `List[i<n] elemTyp`,
- `f` has type `![0] (forall step. (accType, elemType{n-(step+1)/i}) -o[m,0] accType{step+1/step})`,
- `acc` has type `accType{0/step}`,

`fold f acc list` has type `accType{n/step}`.

**Note:** Sometimes there is no list to fold over, but it is still necessary to iterate some function `n` times. In these cases, iteration can be simulated by folding the step function over a list of unit values of length `n`. Such a list can be generated using the `range` library function, of type `forall n. List[_<n] ()`.