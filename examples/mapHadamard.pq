mapHadamard :: !forall n. List[_<n] Qubit -o List[_<n] Qubit
mapHadamard n list =
    let hadaStep = lift forall step. \(qs ,q) :: (List[_<step] Qubit, Qubit). qs: (force hadamard @0) q
    in fold(hadaStep , [], list)

main = (force mapHadamard @4) [force qinit0, force qinit0, force qinit0, force qinit0]