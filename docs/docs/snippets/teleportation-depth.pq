--- Quantum Teleportation with depth analysis

-- teleportation-depth.pq

-- put q and p into the entangled |+> state
bell :: !((Qubit{0}, Qubit{0}) -o (Qubit{2}, Qubit{2}))
bell (q, p) = 
    let q = (force hadamard @0) q in
    let (q,p) = (force cnot @1 @0) q p in
    (q,p)

-- Alice's part of the teleportation protocol
alice :: !((Qubit{2}, Qubit{0}) -o (Bit{4}, Bit{5}))
alice (p, r) =
    let (r,p) = (force cnot @0 @2) r p in
    let r = (force hadamard @3) r in
    let c = (force meas @3) p in
    let d = (force meas @4) r in
    (c,d)

-- Bob's part of the teleportation protocol
bob :: !((Qubit{2}, Bit{4}, Bit{5}) -o Qubit{6})
bob (q, c, d) =
    let (c,q) = (force ccnot @4 @2) c q in
    let (d,q) = (force ccz @5 @5) d q in
    let _ = (force cdiscard @5) c  in
    let _ = (force cdiscard @6) d  in
    q

-- teleport the state of qubit r (at depth i) into qubit q
teleport :: !(Qubit{0} -o Qubit{6})
teleport r = 
    let q = force qinit0 in
    let p = force qinit0 in
    let (q,p) = force bell (q,p) in
    let (c,d) = force alice (p,r) in
    force bob (q,c,d)