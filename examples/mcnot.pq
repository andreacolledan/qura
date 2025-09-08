qinit0Many = forall n.
  let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
  in fold(qinitStep, [], force range @n)

qdiscardMany = forall n.\list::List[_<n] Qubit. 
  let qdiscardstep = lift forall step. \(_,q) :: ((),Qubit). (force qdiscard @0) q
  in fold(qdiscardstep, (), list)

rev = forall n.\list::List[_<n] Qubit.
  let revStep = lift forall step.\(qs, q)::(List[_<step] Qubit, Qubit). qs:q
  in fold(revStep, [], list)

qzip = forall n . \(a,b) :: (List[_<n+1] Qubit, List[_<n+1] Qubit).
  let qzipStep = lift forall step. \((llist,reslist),relem) :: ((List[_<(n+1)-step] Qubit, List[_<step] (Qubit,Qubit)), Qubit).
    let ls:lelem = (llist !:: List[_<(n-step)+1] Qubit) in -- we can assume this because zipstep is only called by fold, where step < n+1
    let reslist = reslist : (lelem, relem) in
    (ls, reslist)
  in let (_, zippedlist) = (fold(qzipStep, (a,[]), b) :: (List[_<0] Qubit, List[_<n+1] (Qubit,Qubit)))
  in zippedlist

cnot = (force cnot @0@0)
toffoli = (force toffoli @0@0@0)


-- actually, is (m+2)cnot, idk how to pass the number of controls and make it still work
mcnot :: !(forall m. (List[_<m+2] Qubit, Qubit) -o (List[_<m+2] Qubit, Qubit))
mcnot m (ctrls, trgt) =
  let restControls:q2:q1 = ctrls in
  let a1 = force qinit0 in -- first ancilla
  --- create the first intermediate value ---
  let (q1,q2,a1) = force toffoli q1 q2 a1 in
  --- forward pass ---
  let (restControls, ancillas) = 
    let toffoliStep = lift forall step. \((cs, as:a), c) :: ((List[_<step] Qubit, List[_<step+1] Qubit), Qubit).
      let new = force qinit0 in
      let (a,c,new) = force toffoli a c new in
      (cs:c, (as:a):new) -- note: those are now reversed
    in fold(toffoliStep, ([],[]:a1), restControls) in  
  --- control the target ---
  let ancillas:lastAncilla = ancillas in -- extract last ancilla
  let (lastAncilla, trgt) = force cnot lastAncilla trgt in -- apply cnot to the target
  --- backwards pass ---
  let revancillas = (force rev @m) ancillas in
  let restrevancillas:a1 = revancillas in
  let ancillas = (force rev @m) restrevancillas in -- sorry for the names
  let ra = (force qzip@(m-1)) (restControls,ancillas) in
  let (restControls, a2) = -- use the last control-ancilla as the next toffoli target and then discard it
    let bwToffoliStep = lift forall step. \((cs,aOld),(c,a)) :: ((List[_<step] Qubit, Qubit),(Qubit,Qubit)).
      let (c,a,aOld) = force toffoli c a aOld in 
      let _ = (force qdiscard @0) aOld in
      (cs:c, a)
    in fold(bwToffoliStep, ([],lastAncilla), ra) in
  let (q1,q2,a1) = force toffoli q1 q2 a1 in
  --- discard ancillas and return the mcnot outputs ---
  let _ = (force qdiscard @0) a1 in
  let _ = (force qdiscard @0) a2 in
  -- let _ = (force qdiscard @0) lastAncilla in
  (restControls:q2:q1, trgt)

main =
  let trgt = force qinit1 in
  let ctrls = force qinit0Many @3 in
  (force mcnot @(3-2)) (ctrls, trgt)
