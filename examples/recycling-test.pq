main =
  let a = force qinit1 in
  let b = force qinit1 in
  let a = (force hadamard @1) a in
  let a = (force hadamard @1) a in
  let a = (force hadamard @1) a in
  let a = (force hadamard @1) a in
  let b = (force hadamard @1) b in
  let _ = (force qdiscard @2) a in
  let _ = (force qdiscard @1) b in
  let x = force qinit0 in
  let x = (force hadamard @2) x in x
  