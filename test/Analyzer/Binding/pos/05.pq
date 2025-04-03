test :: !(List[i<10] Qubit -o (Qubit, List[i<9] Qubit))
test (qs:q) = (q, qs)

test2 = \(qs:q)::List[i<10] Qubit . (q, qs)