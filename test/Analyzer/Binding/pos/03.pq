test :: !((Qubit, Bit, ()) -o ((), Bit, Bit))
test (q,b,_) = ((), b, apply(Meas @0, q))

test2 = \(q,b,_)::(Qubit, Bit, ()) . ((), b, apply(Meas @0, q))