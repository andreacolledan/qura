-- All linear resources are correctly consumed
test :: !(Qubit -o (Qubit, Qubit))
test q = let p = apply(QInit0, ()) in (q,p)