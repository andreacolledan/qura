test :: !(((Qubit, Bit), (Qubit, ())) -o (Qubit, Qubit, Bit))
test ((q1, b), (q2, _)) = (q1, q2, b)


test2 = \((q1, b), (q2, _)) :: ((Qubit, Bit), (Qubit, ())) . (q1, q2, b)