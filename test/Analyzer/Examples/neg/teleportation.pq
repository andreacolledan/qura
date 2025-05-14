--- WRONG IMPLEMENTATION FOR TESTING PURPOSES
--- Quantum Teleportation with width and depth analysis

-- teleportation-depth-generic.pq

-- put q and p into the entangled |+> state
bell :: ![0](forall[0,0] dq. forall[0,0] dp.
  (Qubit{dq}, Qubit{dp}) -o[2,0] (Qubit{max(dq+1, dp) + 1}, Qubit{max(dq+1, dp) + 1}))
bell dq dp (q, p) = 
    let q = (force hadamard @dq) q in
    let (q,p) = (force cnot @dq+1 @dp) q p in
    (q,p)

-- Alice's part of the teleportation protocol
alice :: ![0](forall[0,0] dp. forall[0,0] dr.
  (Qubit{dp}, Qubit{dr}) -o[2,0] (Bit{max(dp, dr) + 2}, Bit{max(dp, dr) + 3}))
alice dp dr (p, r) =
    let (r,p) = (force cnot @dr @dp) r p in
    let r = (force hadamard @ max(dp, dr) + 1) r in
    let c = (force meas @ max(dp, dr) + 1) p in
    let d = (force meas @ max(dp, dr) + 2) r in
    (c,d)

-- Bob's part of the teleportation protocol
bob :: ![0](forall[0,0] dq. forall[0,0] dc. forall[0,0] dd.
  (Qubit{dq}, Bit{dc}, Bit{dd}) -o[3,0] Qubit{max(dd, max(dc, dq) + 1) + 1})
bob dq dc dd (q, c, d) =
    let (c,q) = (force ccnot @dc @dq) c q in
    let (d,q) = (force ccz @dd @ max(dc, dq) + 1) d q in
    let _ = (force cdiscard @ max(dc, dq) + 1) c  in
    let _ = (force cdiscard @ max(dd, max(dc, dq) + 1) + 1) d  in
    q

-- teleport the state of qubit r (at depth i) into qubit q
-- Width should be 3 and depth should be dr+6
teleport :: ![0](forall[0,0] dr. Qubit{dr} -o[2,0] Qubit{dr+5})
teleport dr r = 
    let q = force qinit0 in
    let p = force qinit0 in
    let (q,p) = (force bell @0 @0) (q,p) in
    let (c,d) = (force alice @2 @dr) (p,r) in
    (force bob @2 @ max(2, dr) + 2 @ max(2, dr) + 3)  (q,c,d)