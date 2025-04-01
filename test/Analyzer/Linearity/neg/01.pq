test :: !(Qubit, Qubit)
test = let q = apply(QInit0, ()) in (q,q)