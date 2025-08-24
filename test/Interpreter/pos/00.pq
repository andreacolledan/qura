main =
  let a = force qinit1 in
  let b = force qinit1 in
  let _ = (force qdiscard @0) a in
  let _ = (force qdiscard @0) b in
  let c = force qinit1 in
  let d = force qinit0 in
  (force cnot @0 @0) c d
  