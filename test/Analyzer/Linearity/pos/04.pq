test :: !(Qubit -o ())
test q = apply(QDiscard @0, q)