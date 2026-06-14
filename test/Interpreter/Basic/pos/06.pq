main :: ! (Qubit, Qubit)
main = let q = apply(QInit0, ()) in
        let p = apply(QInit1, ()) in
        (q, p)