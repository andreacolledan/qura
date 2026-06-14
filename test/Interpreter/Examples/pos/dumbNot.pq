-- negate a qubit q at depth d in a dumb way,
-- by initializing an ancillary qubit a to 1 and
-- applying a cnot with target q and control a
dumbNot :: ![0](forall[0] d. Qubit{d} -o[2] Qubit{d+1})
dumbNot d q =
  let a = force qinit1 in
  let (a,q) = (force cnot @0 @d) a q in
  let _ = (force qdiscard @(d+1)) a in
  q

-- negate twice a qubit y at depth x, using dumbNot
-- this is useless, but highlights qubit reuse
doubleNegation :: ![0](forall[0] d. Qubit{d} -o[2] Qubit{d+2})
doubleNegation d q =
  (force dumbNot @(d+1)) ((force dumbNot @d) q)

main :: ![2] Qubit{2}
main = let q = force qinit0 in (force doubleNegation @ 0) q