mapHadamard :: ![0](forall [0 ,0] d. forall [0 ,0] n. (List[_<n] Qubit{d}) -o[n ,0] (List[_<n] Qubit{d+1}))
mapHadamard d n list =
    let hadaStep = lift forall step. \(qs ,q) :: (List[_<step] Qubit {d+1}, Qubit {d}). qs: (force hadamard @d) q
    in fold(hadaStep , [], list)

main = (force mapHadamard @0 @4) [force qinit0, force qinit0, force qinit0, force qinit0]