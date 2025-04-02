-- Succeeds because linear variable is only locally shadowed
test :: !(Qubit -o (Qubit, Qubit))
test q = (let q = apply(QInit0, ()) in q, q)