-- teleportation.pq

-- put q and p into the entangled |+> state
bell :: !((Qubit, Qubit) -o (Qubit, Qubit))
bell (b, a) = 
    let b = (force hadamard @0) b in
    let (b,a) = (force cnot @0 @0) b a in
    (b,a)

-- Alice's part of the teleportation protocol
alice :: !((Qubit, Qubit) -o (Bit, Bit))
alice (a, q) =
    let (q,a) = (force cnot @0 @0) q a in
    let q = (force hadamard @0) q in
    let y = (force meas @0) a in
    let x = (force meas @0) q in
    (y,x)

-- Bob's part of the teleportation protocol
bob :: !((Qubit, Bit, Bit) -o Qubit)
bob (b, y, x) =
    let (y,b) = (force ccnot @0 @0) y b in
    let (x,b) = (force ccz @0 @0) x b in
    let _ = (force cdiscard @0) y  in
    let _ = (force cdiscard @0) x  in
    b

-- teleport the state of qubit q (at depth i) into qubit b
teleport :: !(Qubit -o Qubit)
teleport q = 
    let a = force qinit0 in
    let b = force qinit0 in
    let (b,a) = force bell (b,a) in
    let (y,x) = force alice (a,q) in
    force bob (b,y,x)

main :: ![0] Qubit{0}
main =
    let q = force qinit1       -- initialize the qubit to |1>
    in (force teleport) q      -- teleport it
