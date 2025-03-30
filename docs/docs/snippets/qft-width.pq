--- Quantum Fourier Transform with width analysis

-- qft-width.pq

--- HELPER FUNCTIONS ---

-- invert the list of intermediate qubits at iteration iter
qrev :: ![0](forall[0,0] iter. List[i<iter] Qubit -o[iter,0] List[i<iter] Qubit)
qrev iter reg =
    let revStep = lift forall step.
      \(rev, q) :: (List[i<step] Qubit, Qubit).
        rev:q in
    fold(revStep, [], reg)

-- apply the controlled rotation gate to the target qubit trg at iteration iter
rotate :: ![0](forall[0,0] iter. ![0](forall[0,0] step.
  ((List[i < step] Qubit, Qubit), Qubit) -o[step+2,0] (List[i<step+1] Qubit, Qubit)))
rotate iter = lift forall step.
  \((ctrls, trg), ctrl)::((List[i<step] Qubit, Qubit), Qubit).
    let (ctrl, trg) = (force cr @(iter+1-step) @0 @0) ctrl trg in
    (ctrls:ctrl, trg)

--- QUANTUM FOURIER TRANSFORM ---

qftIteration :: ![0](forall[0,0] iter. (List[i<iter] Qubit, Qubit) -o[iter+1,0] List[i<iter+1] Qubit)
qftIteration iter (ctrls, trg) =
    let revctrls = (force qrev @iter) ctrls in
    let (ctrls, trg) = fold(force rotate @iter, ([], trg), revctrls) in
    let trg = (force hadamard @0) trg in
    ctrls:trg

-- apply the Quantum Fourier Transform to n qubits at depth d
qft :: ![0](forall[0,0] n. List[i<n] Qubit -o[n,0] List[i<n] Qubit)
qft n reg = fold(qftIteration, [], reg)