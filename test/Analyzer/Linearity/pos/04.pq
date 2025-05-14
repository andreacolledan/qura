-- All linear resources are consumed (discarded explicitly)
test :: !(Qubit -o ())
test q = apply(QDiscard @0, q)