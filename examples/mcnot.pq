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
mcnot :: ![0](forall[0,0] n. forall[0,0] dc. forall[0,0] dt. List[_<n+2] Qubit{dc} -o[0,0] Qubit{dt} -o[2*(n+2),n+2] (List[_<n+2] Qubit{max(dc+n+2,dt+1)+n+2}, Qubit{max(1+dc+n+1,1+dt)}))
mcnot n dc dt ctrls trgt =
  let restControls:q2:q1 = ctrls in
  let a1 = force qinit0 in -- first ancilla
  --- create the first intermediate value ---
  let (q1,q2,a1) = (force toffoli @dc@dc@0) q1 q2 a1 in
  --- forward pass ---
  let toffoliStep = lift forall step. \((cs, as, a), c) :: ((List[_<step] Qubit{dc+step+2}, List[_<step] Qubit{dc+step+2}, Qubit{dc+step+1}), Qubit{dc}).
    let new = force qinit0 in
    let (a,c,new) = (force toffoli @max(1,dc+1)+step @dc @0) a c new in
    (cs:c, as:a, new)
  in let (restControls, ancillas, lastAncilla) = fold(toffoliStep, ([],[],a1), restControls) in 
  --- control the target ---
  -- let ancillas:lastAncilla = ancillas in -- extract last ancilla
  let (lastAncilla, trgt) = (force cnot @dc+n+1 @dt) lastAncilla trgt in -- apply cnot to the target
  --- backwards pass ---
  let ca = (force qzip @n @dc+n+2) ((force rev @n @dc+n+2) restControls, (force rev @n @dc+n+2)ancillas) in -- zip controls and ancillas
  let bwToffoliStep = lift forall step. \((cs,nextTarget),(c,a)) :: ((List[_<step] Qubit{max(dc+n+2,dt+1)+step+2}, Qubit{max(dc+n+2,dt+1)+step+1}),(Qubit{dc+n+2},Qubit{dc+n+2})).
      let (c,a,nextTarget) = (force toffoli @dc+n+2 @dc+n+2 @max(dc+n+2,dt+1)+step+1) c a nextTarget in
      let _ = (force qdiscard @max(dc+n+2,dt+1)+step+2) nextTarget in
      (cs:c, a)
  in let (restControls, a1) = fold(bwToffoliStep, ([],lastAncilla), ca) in
  let (q1,q2,a1) = (force toffoli @dc+1 @dc+1 @max(dc+n+2,dt+1)+n+1) q1 q2 a1 in
  let _ = (force qdiscard @max(dc+n+2,dt+1)+n+2) a1 in
  --- return the mcnot outputs ---
  (restControls:q2:q1, trgt) -- reorder the controls

--- MAIN
-- qinit0Many = forall n.
--   let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
--   in fold(qinitStep, [], force range @n)
-- main =
--   let trgt = force qinit1 in
--   let ctrls = force qinit0Many @6 in
--   (force mcnot @(6-2) @0 @0) ctrls trgt
