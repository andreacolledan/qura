--- Quantum Teleportation with width and depth analysis

--- Parameters:
--- d : initial depth of the teleported qubit

-- put q and p into the entangled |+> state
bell :: ![0](forall[0,0] d.(Qubit{d}, Qubit{d}) -o[2,0] (Qubit{d+2}, Qubit{d+2}))
bell d (q, p) = 
    let q = (force hadamard @d) q in
    let (q,p) = (force cnot @(d+1) @d) q p in
    (q,p)

-- Alice's part of the teleportation protocol
alice :: ![0](forall[0,0] d. (Qubit{2}, Qubit{d}) -o[2,0] (Bit{2 + max(2, d)}, Bit{3 + max(2, d)}))
alice d (p, r) =
    let (r,p) = (force cnot @d @2) r p in
    let r = (force hadamard @(1+max(2,d))) r in
    let c = (force meas @(1+max(2,d))) p in
    let d = (force meas @(2+max(2,d))) r in
    (c,d)

-- Bob's part of the teleportation protocol
bob :: ![0](forall[0,0] d. (Qubit{2}, Bit{2 + max(2, d)}, Bit{3 + max(2, d)}) -o[3,0] Qubit{4 + max(2, d)})
bob  d (q, c, d) =
    let (c,q) = (force ccnot @(2+max(2,d)) @2) c q in
    let (d,q) = (force ccz @(3+max(2,d)) @(3+max(2,d))) d q in
    let _ = (force cdiscard @(3+max(2,d))) c  in
    let _ = (force cdiscard @(4+max(2,d))) d  in
    q

-- teleport the state of qubit r (at depth i) into qubit q
teleport :: ![0](forall[0,0] d. Qubit{d} -o[3,0] Qubit{d + 6})
teleport d r = 
    -- set-up
    let q = force qinit0 in
    let p = force qinit0 in
    let (q,p) = (force bell @0) (q,p) in
    -- Alice gets p and r, Bob gets q, Alice travels away from Bob
    let (c,d) = (force alice @d) (p,r) in
    -- classical communication of c,d happens here
    ((force bob @d) (q,c,d) :: Qubit{d+6})