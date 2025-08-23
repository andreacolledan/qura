bell :: !((Qubit, Qubit) -o (Qubit, Qubit))
bell (q1, q2) = let q1 = apply(Hadamard @0, q1) in
                let (q1, q2) = apply(CNot @0 @0, (q1, q2)) in
                (q1, q2)

main :: !(Qubit, Qubit)
main = let q = apply(QInit0, ()) in
        let p = apply(QInit1, ()) in
        (force bell) (q, p)