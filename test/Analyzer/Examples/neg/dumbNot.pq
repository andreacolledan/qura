--- WRONG IMPLEMENTATION FOR TESTING PURPOSES
-- negate a qubit q at depth d in a dumb way,
-- by initializing an ancillary qubit a to 1 and
-- applying a cnot with target q and control a
dumbNot :: ![0](forall[0,0] d. Qubit{d} -o[2,0] Qubit{d+1})
dumbNot d q =
  let a = force qinit1 in
  let (a,q) = (force cnot @0 @d) a q in
  let _ = (force qdiscard @(d+1)) a in
  q

-- negate twice a qubit q at depth d, using dumbNot
-- this is useless, but highlights qubit reuse
-- Both width and depth are off by one
doubleNegation :: ![0](forall[0,0] d. Qubit{d} -o[1,0] Qubit{d+1})
doubleNegation d q =
  (force dumbNot @(d+1)) ((force dumbNot @d) q)