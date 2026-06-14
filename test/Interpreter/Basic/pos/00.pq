main :: ![2](Qubit{1})
main =
  let a = force qinit1 in
  let b = force qinit1 in
  let _ = (force qdiscard @1) a in
  let _ = (force qdiscard @1) b in
  let x = force qinit1 in x
  -- let d = force qinit0 in
  -- (force cnot @0 @0) c d
  