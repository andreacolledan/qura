bell :: !((Qubit, Qubit) -o (Qubit, Qubit))
bell (q1, q2) = let q1 = apply(Hadamard @0, q1) in
                let (q1, q2) = apply(CNot @0 @0, (q1, q2)) in
                (q1, q2)

initBellState :: !(() -o (Qubit, Qubit))
initBellState _ = let q = apply(QInit0, ()) in
                  let p = apply(QInit1, ()) in
                  let boxedBell = box bell in
                  apply(boxedBell, (q, p))

main :: !(Qubit, Qubit)
main = let boxedInitBellState = box initBellState in
        apply(boxedInitBellState, ())