--- Quantum Fourier Transform without resource analysis

-- qft.pq

--- HELPER FUNCTIONS ---

-- invert the list of intermediate qubits at iteration iter
qrev :: !(forall iter. List[i<iter] Qubit -o List[i<iter] Qubit)
qrev iter reg =
    let revStep = lift forall step.
      \(rev, q) :: (List[i<step] Qubit, Qubit).
        rev:q in
    fold(revStep, [], reg)

-- apply the controlled rotation gate to the target qubit trg at iteration iter
rotate :: !(forall iter. !(forall step.
  ((List[i < step] Qubit, Qubit), Qubit) -o (List[i < step + 1] Qubit, Qubit)))
rotate iter = lift forall step.
  \((ctrls, trg), ctrl)::((List[i<step] Qubit, Qubit), Qubit).
    let (ctrl, trg) = (force cr @(iter+1-step) @0 @0) ctrl trg in
    (ctrls:ctrl, trg)

--- QUANTUM FOURIER TRANSFORM ---

qftIteration :: !(forall iter. (List[i<iter] Qubit, Qubit) -o List[i<iter+1] Qubit)
qftIteration iter (ctrls, trg) =
    let revctrls = (force qrev @iter) ctrls in
    let (ctrls, trg) = fold(force rotate @iter, ([], trg), revctrls) in
    let trg = (force hadamard @0) trg in
    ctrls:trg

-- apply the Quantum Fourier Transform to n qubits at depth d
qft :: !(forall n. List[i<n] Qubit -o List[i<n] Qubit)
qft n reg = fold(qftIteration, [], reg)