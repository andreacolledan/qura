rev = forall n. forall d. \list::List[_<n] Qubit{d}.
  let revStep = lift forall step.\(qs, q)::(List[_<step] Qubit{d}, Qubit{d}). qs:q
  in fold(revStep, [], list)
qzip = forall n. forall d. \(a,b) :: (List[_<n] Qubit{d}, List[_<n] Qubit{d}).
  let qzipStep = forall m. lift forall step. \((llist,rlist,reslist),_) :: ((List[_<((m+1)-step)] Qubit{d}, List[_<((m+1)-step)] Qubit{d}, List[_<step] (Qubit{d},Qubit{d})), ()).
    let ls:lelem = (llist !:: List[_<(m-step)+1] Qubit{d}) in -- we can assume this because zipstep is only called by fold, where step < n = m+1
    let rs:relem = (rlist !:: List[_<(m-step)+1] Qubit{d}) in -- same here
    let reslist = reslist : (lelem, relem) in
    (ls, rs, reslist)
  in let (_,_, zippedlist) = (fold(qzipStep @ n-1, (a,b,[]), force range @n))
  in zippedlist

--- MCNOT
-- actually, is (n+2)cnot, so n = m-2
mcnot :: ![0](forall[0,0] n. forall[0,0] dc. forall[0,0] dt. List[_<n+2] Qubit{dc} -o[0,0] Qubit{dt} -o[2*(n+2),n+2] (List[_<n+2] Qubit{dc}, Qubit{dt}))
mcnot n dc dt ctrls trgt =
  
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
  let (lastAncilla, trgt) = (force cnot @dc+n @dc+n) lastAncilla trgt in -- apply cnot to the target
  --- backwards pass ---
  let revancillas = (force rev @n @dc+n) ancillas in
  let revancillas = revancillas:q2 in -- add the second control to the list of ancillas
  let revrestControls = (force rev @n @dc+n) restControls in
  let revrestControls = revrestControls:q1 in -- add the first control to the list of controls
  let ca = (force qzip @n+1 @dc+n) (revrestControls,revancillas) in -- zip controls and ancillas
  let (restControls:q1, q2) = -- use the last control-ancilla as the next toffoli target and then discard it
    let bwToffoliStep = lift forall step. \((cs,aOld),(c,a)) :: ((List[_<step] Qubit{dc+step}, Qubit{dc+step}),(Qubit{dc+step},Qubit{dc+step})).
      let (c,a,aOld) = (force toffoli @dc+n+step @dc+n+step @dc+n+step) c a aOld in 
      let _ = (force qdiscard @0) aOld in
      (cs:c, a)
    in fold(bwToffoliStep, ([],lastAncilla), ca) in
  --- return the mcnot outputs ---
  (restControls:q2:q1, trgt) -- reorder the controls

--- MAIN
qinit0Many = forall n.
  let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
  in fold(qinitStep, [], force range @n)
-- main =
--   let trgt = force qinit1 in
--   let ctrls = force qinit0Many @4 in
--   (force mcnot @(4-2) @0 @0) ctrls trgt
