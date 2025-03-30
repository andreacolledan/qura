--- Quantum Teleportation with width analysis

-- teleportation-width.pq

-- put q and p into the entangled |+> state
bell :: ![0]((Qubit, Qubit) -o[2,0] (Qubit, Qubit))
bell (q, p) = 
    let q = (force hadamard @0) q in
    let (q,p) = (force cnot @0 @0) q p in
    (q,p)

-- Alice's part of the teleportation protocol
alice :: ![0]((Qubit, Qubit) -o[2,0] (Bit, Bit))
alice (p, r) =
    let (r,p) = (force cnot @0 @0) r p in
    let r = (force hadamard @0) r in
    let c = (force meas @0) p in
    let d = (force meas @0) r in
    (c,d)

-- Bob's part of the teleportation protocol
bob :: ![0]((Qubit, Bit, Bit) -o[3,0] Qubit)
bob (q, c, d) =
    let (c,q) = (force ccnot @0 @0) c q in
    let (d,q) = (force ccz @0 @0) d q in
    let _ = (force cdiscard @0) c  in
    let _ = (force cdiscard @0) d  in
    q

-- teleport the state of qubit r (at depth i) into qubit q
teleport :: ![0](Qubit -o[3,0] Qubit)
teleport r = 
    let q = force qinit0 in
    let p = force qinit0 in
    let (q,p) = force bell (q,p) in
    let (c,d) = force alice (p,r) in
    force bob (q,c,d)