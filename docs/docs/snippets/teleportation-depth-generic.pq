--- Quantum Teleportation with depth analysis

-- teleportation-depth-generic.pq

-- put q and p into the entangled |+> state
bell :: !(forall dq. forall dp.
  (Qubit{dq}, Qubit{dp}) -o (Qubit{max(dq+1, dp) + 1}, Qubit{max(dq+1, dp) + 1}))
bell dq dp (q, p) = 
    let q = (force hadamard @dq) q in
    let (q,p) = (force cnot @dq+1 @dp) q p in
    (q,p)

-- Alice's part of the teleportation protocol
alice :: !(forall dp. forall dr.
  (Qubit{dp}, Qubit{dr}) -o (Bit{max(dp, dr) + 2}, Bit{max(dp, dr) + 3}))
alice dp dr (p, r) =
    let (r,p) = (force cnot @dr @dp) r p in
    let r = (force hadamard @ max(dp, dr) + 1) r in
    let c = (force meas @ max(dp, dr) + 1) p in
    let d = (force meas @ max(dp, dr) + 2) r in
    (c,d)

-- Bob's part of the teleportation protocol
bob :: !(forall dq. forall dc. forall dd.
  (Qubit{dq}, Bit{dc}, Bit{dd}) -o Qubit{max(dd, max(dc, dq) + 1) + 1})
bob dq dc dd (q, c, d) =
    let (c,q) = (force ccnot @dc @dq) c q in
    let (d,q) = (force ccz @dd @ max(dc, dq) + 1) d q in
    let _ = (force cdiscard @ max(dc, dq) + 1) c  in
    let _ = (force cdiscard @ max(dd, max(dc, dq) + 1) + 1) d  in
    q

-- teleport the state of qubit r (at depth i) into qubit q
teleport :: !(forall dr. Qubit{dr} -o Qubit{dr+6})
teleport dr r = 
    let q = force qinit0 in
    let p = force qinit0 in
    let (q,p) = (force bell @0 @0) (q,p) in
    let (c,d) = (force alice @2 @dr) (p,r) in
    (force bob @2 @ max(2, dr) + 2 @ max(2, dr) + 3)  (q,c,d)