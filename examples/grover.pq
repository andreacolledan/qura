--- Grover search algorithm with depth and size analysis ---
--- Following the circuit description at:
--- https://cnot.io/quantum_algorithms/grover/grovers_algorithm.html

--- HELPER FUNCTIONS ---

-- initialize n qubits to |0>
qinit0Many = forall n.
  let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
  in fold(qinitStep, [], force range @n)

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
diffusion :: ![0](forall[0,0] n. forall[0,0] d. List[_<n] Qubit{d} -o[0,0] Qubit{d} -o[4*n + 1, 0] (List[_<n] Qubit{d+5}, Qubit{d+3}))
diffusion n d reg a =
    let reg = (force mapHadamard @ n @ d) reg in
    -- begin negatively controlled not
    let reg = (force mapQnot @ n @ d+1) reg in
    let (reg,a) = (force mcnot @ n @ d+2 @ d) reg a in
    let reg = (force mapQnot @ n @ d+3) reg in
    -- end negatively controlled not
    let reg = (force mapHadamard @ n @ d+4) reg in
    (reg, a)

-- perform a single grover iteration on n qubits at depth d,
-- using an ancilla a at depth d
-- and an oracle of width os and depth od
groverIteration :: ![0](forall[0,0] n. forall[0,0] d. forall[0,0] os. forall[0,0] od.
  (forall[0,0] d. Circ[os]((List[_<n] Qubit{d}, Qubit{d}), (List[_<n] Qubit{d + od}, Qubit{d + od})))
  -o[0,0] List[_<n] Qubit{d} -o[0,0] Qubit{d}
  -o[os + 4*n + 1, 0] (List[_<n] Qubit{d + od + 5}, Qubit{d + od + 3}))
groverIteration n d os od oracle reg a = 
    let (reg, a) = apply(oracle @ d, (reg, a)) in
    (force diffusion @ n @ d + od) reg a

-- run Grover's algorithm on an oracle of input size n, width os and depth od
grover :: ![0](forall[0, 0] r. forall[0, 0] n. forall[0, 0] os. forall[0, 0] od.
  (forall[0,0] d. Circ[os]((List[_<n] Qubit{d}, Qubit{d}), (List[_<n] Qubit{d+od}, Qubit{d+od})))
  -o[2*n + r*(os + 4*n + 1) + 1, 0] List[_<n] Bit{2 + r*(od+5)})
grover r n os od oracle =
    -- prepare working qubits
    let wqs = force qinit0Many @ n in
    let wqs = (force mapHadamard @ n @ 0) wqs in
    -- prepare ancilla
    let a = force qinit1 in
    let a = (force hadamard @ 0) a in
    -- iterate Grover's algorithm
    let iteration = lift forall step. \((wqs, a), _) :: ((List[_<n] Qubit{1+step*(od+5)}, Qubit{1+step*(od+5)}), ()).
      (force groverIteration @ n @ 1+step*(od+5) @ os @ od) oracle wqs a
    in
    let (wqs, a) = fold(iteration, (wqs, a), force range @r) in
    let _ = (force qdiscard @ 1 + r * (od + 5)) a in
    (force mapMeasure @ n @ 1 + r * (od + 5)) wqs

--- ORACLES ---

-- The oracle conj : {0,1}^3 -> {0,1} such that conj(x) = 1 <==> x = 111
conj = forall d.
  box $ lift \(reg, a) :: (List[_<3] Qubit{d}, Qubit{d}).
    (force mcnot @ 3 @ d @ d) reg a -- Ancilla is flipped iff reg is 111

-- The oracle and : {0,1}^2 -> {0,1} such that and(x) = 1 <==> x = 11
and = forall d.
  box $ lift \(reg, a) :: (List[_<2] Qubit{d}, Qubit{d}).
    (force mcnot @ 2 @ d @ d) reg a -- Ancilla is flipped iff reg is 111