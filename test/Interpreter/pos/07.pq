main :: ! (Qubit, Qubit)
main = let q = apply(QInit0, ()) in
        let p = apply(QInit1, ()) in
        let qlist = [p, q] in
        let (_:p:q) = qlist in
        (q, p)