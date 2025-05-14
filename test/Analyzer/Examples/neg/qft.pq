--- WRONG IMPLEMENTATION FOR TESTING PURPOSES
--- Quantum Fourier Transform with width and depth analysis

--- Parameters:
--- n     : size of the input to the QFT
--- d     : initial depth of the input qubits
--- iter  : current iteration of the QFT


--- HELPER FUNCTIONS ---

-- invert the list of intermediate qubits at iteration iter
qrev = forall iter. forall d.
  \reg :: List[i<iter] Qubit{d+iter+i}.
    let revStep = lift forall step.
      \(rev, q) :: (List[i<step] Qubit{d+2*iter-(i+1)}, Qubit{d+2*iter-(step+1)}).
        rev:q in
    fold(revStep, [], reg)

-- apply the controlled rotation gate to the target qubit trg at iteration iter
rotate = forall d. forall iter. lift forall step.
  \((ctrls, trg), ctrl)::((List[i<step] Qubit{d+iter+i+1}, Qubit{d+iter+step}), Qubit{d+iter+step}).
    let (ctrl, trg) = (force cr @(iter+1-step) @(d+iter+step) @(d+iter+step)) ctrl trg in
    (ctrls:ctrl, trg) -- :: (List[i<step+1] Qubit{d+iter+i+1}, Qubit{d+iter+step+1})


--- QUANTUM FOURIER TRANSFORM ---

-- apply the Quantum Fourier Transform to n qubits at depth d
-- Width should actually be linear and equal to n. Depth should be d+n+i
qft :: ![0](forall[0,0] n. forall[0,0] d. List[i<n] Qubit{d} -o[5,0] List[i<n] Qubit{d+n})
qft n d reg =
    let qftIter = lift forall iter. -- define the iteration of the QFT
      \(ctrls, trg)::(List[i<iter] Qubit{d+iter+i}, Qubit{d}).
        let revctrls = (force qrev @iter @d) ctrls in -- List[i<iter] Qubit{d+2*iter-(i+1)}
        let (ctrls, trg) = fold(force rotate @d @iter, ([], trg), revctrls) in
        -- note (ctrls, trg) :: (List[i<iter] Qubit{d+iter+i+1}, Qubit{d+2*iter})
        let trg = (force hadamard @(d+2*iter)) trg in
        ctrls:trg -- List[i<iter+1] Qubit{d+iter+1+i}
    in fold(qftIter, [], reg) -- List[i<n] Qubit{d+n+i}