--- Grover search algorithm with depth and size analysis ---
--- Following the circuit description at:
--- https://cnot.io/quantum_algorithms/grover/grovers_algorithm.html

--- HELPER FUNCTIONS ---
-- initialize n qubits to |0>
qinit0Many = forall n.
  let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
  in fold(qinitStep, [], force range @n)

rev = forall n. forall d. \list::List[_<n] Qubit{d}.
  let revStep = lift forall step.\(qs, q)::(List[_<step] Qubit{d}, Qubit{d}). qs:q
  in fold(revStep, [], list)

qzip = forall n. forall d. \(a,b) :: (List[_<n+1] Qubit{d}, List[_<n+1] Qubit{d}).
  let qzipStep = lift forall step. \((llist,reslist),relem) :: ((List[_<(n+1)-step] Qubit{d}, List[_<step] (Qubit{d},Qubit{d})), Qubit{d}).
    let ls:lelem = (llist !:: List[_<(n-step)+1] Qubit{d}) in -- we can assume this because zipstep is only called by fold, where step < n+1
    let reslist = reslist : (lelem, relem) in
    (ls, reslist)
  in let (_, zippedlist) = (fold(qzipStep, (a,[]), b) :: (List[_<0] Qubit{d}, List[_<n+1] (Qubit{d},Qubit{d})))
  in zippedlist

-- cnot = (force cnot @0@0)
-- toffoli = (force toffoli @0@0@0)

-- actually, is (m+2)cnot, idk how to pass the number of controls and make it still work
mcnot :: ![0](forall[0,0] m. forall[0,0] dc. forall[0,0] dt. List[_<m+2] Qubit{dc} -o[0,0] Qubit{dt} -o[2*m + 4,m+2] (List[_<m+2] Qubit{dc}, Qubit{dt}))
mcnot m dc dt ctrls trgt =
  let restControls:q2:q1 = ctrls in
  let a1 = force qinit0 in -- first ancilla
  --- create the first intermediate value ---
  let (q1,q2,a1) = (force toffoli @dc@dc@0) q1 q2 a1 in
  --- forward pass ---
  let (restControls, ancillas) = 
    let toffoliStep = lift forall step. \((cs, as:a), c) :: ((List[i<step] Qubit{dc+i+1}, List[i<step+1] Qubit{dc+i}), Qubit{dc}).
      let new = force qinit0 in
      let (a,c,new) = (force toffoli @dc+step @dc @0) a c new in
      (cs:c, (as:a):new)
    in fold(toffoliStep, ([],[]:a1), restControls) in  
  --- control the target ---
  let ancillas:lastAncilla = ancillas in -- extract last ancilla
  let (lastAncilla, trgt) = (force cnot @dc+m @dc+m) lastAncilla trgt in -- apply cnot to the target
  --- backwards pass ---
  let revancillas = (force rev @m @dc+m) ancillas in
  let revancillas = revancillas:q2 in -- add the second control to the list of ancillas
  let revrestControls = (force rev @m @dc+m) restControls in
  let revrestControls = revrestControls:q1 in -- add the first control to the list of controls
  let ca = (force qzip @m @dc+m) (revrestControls,revancillas) in -- zip controls and ancillas
  let (restControls:q1, q2) = -- use the last control-ancilla as the next toffoli target and then discard it
    let bwToffoliStep = lift forall step. \((cs,aOld),(c,a)) :: ((List[_<step] Qubit{dc+step}, Qubit{dc+step}),(Qubit{dc+step},Qubit{dc+step})).
      let (c,a,aOld) = (force toffoli @dc+m+step @dc+m+step @dc+m+step) c a aOld in 
      let _ = (force qdiscard @0) aOld in
      (cs:c, a)
    in fold(bwToffoliStep, ([],lastAncilla), ca) in
  --- return the mcnot outputs ---
  (restControls:q2:q1, trgt) -- reorder the controls

-- apply the Hadamard gate to n qubits at depth d
mapHadamard = forall n . forall d. \x :: List[_<n] Qubit{d} .
  let hadamardStep = lift forall step . \(qs,q) :: (List[_<step] Qubit{d+1}, Qubit{d}). qs:((force hadamard @ d) q)
  in fold(hadamardStep, [], x)

-- apply the X gate to n qubits at depth d
mapQnot = forall n. forall d . \x :: List[_<n] Qubit{d} .
  let qnotStep = lift forall step . \(qs,q) :: (List[_<step] Qubit{d+1}, Qubit{d}). qs:((force qnot @d) q)
  in fold(qnotStep, [], x)

-- measure n qubits at depth d in the computational basis
mapMeasure = forall n. forall d. \x :: List[_<n] Qubit{d}.
  let measureStep = lift forall step. \(bs,q) :: (List[_<step] Bit{d+1}, Qubit{d}). bs:((force meas @d) q)
  in fold(measureStep, [], x)

--- GROVER'S ALGORITHM ---
-- the diffusion operator on n qubits with ancilla a (all qubits at depth d)
diffusion :: ![0](forall[0,0] n. forall[0,0] d. List[_<n+2] Qubit{d} -o[0,0] Qubit{d} -o[2*n+4, n+2] (List[_<n+2] Qubit{d+5}, Qubit{d+3}))
diffusion n d reg a =
  let reg = (force mapHadamard @ n+2 @ d) reg in
  -- begin negatively controlled not
  let reg = (force mapQnot @ n+2 @ d+1) reg in
  let (reg,a) = (force mcnot @ n @ d+2 @ d) reg a in
  let reg = (force mapQnot @ n+2 @ d+3) reg in
  -- end negatively controlled not
  let reg = (force mapHadamard @ n+2 @ d+4) reg in
  (reg, a)

-- perform a single grover iteration on n qubits at depth d,
-- using an ancilla a at depth d
-- and an oracle of width os and depth od
groverIteration :: ![0](forall[0,0] n. forall[0,0] d. forall[0,0] os. forall[0,0] od.
  (forall[0,0] d. Circ[os]((List[_<n+2] Qubit{d}, Qubit{d}), (List[_<n+2] Qubit{d + od}, Qubit{d + od})))
  -o[0,0] List[_<n+2] Qubit{d} -o[0,0] Qubit{d}
  -o[max(max(n + 3, os), 2*n+4), n + 2] (List[_<n+2] Qubit{d + od + 5}, Qubit{d + od + 3}))
groverIteration n d os od oracle reg a = 
  let (reg, a) = apply(oracle @ d, (reg, a)) in
  (force diffusion @ n @ d + od) reg a

-- run Grover's algorithm on an oracle of input size n, width os and depth od
grover :: ![0](forall[0, 0] r. forall[0, 0] n. forall[0, 0] os. forall[0, 0] od.
  (forall[0,0] d. Circ[os]((List[_<n+2] Qubit{d}, Qubit{d}), (List[_<n+2] Qubit{d+od}, Qubit{d+od})))
  -o[max(max(n + 3, os), 2*n+4), 0] List[_<n+2] Bit{2 + r*(od+5)})  
grover r n os od oracle =
  -- prepare working qubits
  let wqs = force qinit0Many @ n+2 in
  let wqs = (force mapHadamard @ n+2 @ 0) wqs in
  -- prepare ancilla
  let a = force qinit1 in
  let a = (force hadamard @ 0) a in
  -- iterate Grover's algorithm
  let iteration = lift forall step. \((wqs, a), _) :: ((List[_<n+2] Qubit{1+step*(od+5)}, Qubit{1+step*(od+5)}), ()).
    (force groverIteration @ n @ 1+step*(od+5) @ os @ od) oracle wqs a
  in
  let (wqs, a) = fold(iteration, (wqs, a), force range @r) in
  let _ = (force qdiscard @ 1 + r * (od + 5)) a in
  (force mapMeasure @ n+2 @ 1 + r * (od + 5)) wqs

--- ORACLES ---
-- The oracle conj : {0,1}^(n+2) -> {0,1} such that conj(x) = 1 <==> x = {1}^(n+2)
conj = forall n. forall d.
  box $ lift \(reg, a) :: (List[_<n+2] Qubit{d}, Qubit{d}).
    (force mcnot @ n @ d @ d) reg a
-- conjgrover perform r iterations on an input register of size n+2 with the oracle at depth od
conjgrover :: ![0](forall [0,0] r. forall[0,0] n. forall[2*n+4,0] od. List[_<n+2] Bit{od+2*n+3}) -- <-- check
conjgrover r n od = ((force grover) @r @n @2*n+4 @od) (force conj @n)