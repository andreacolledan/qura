-- adder.pq annotated with width and depth
-- reverses a list of n qubits
rev = forall n. forall d. \list::List[_<n] Qubit{d}.
  let revStep = lift forall step.\(qs, q)::(List[_<step] Qubit{d}, Qubit{d}). qs:q
  in fold(revStep, [], list)
-- reverses a list of n pairs of qubits
revpair = forall n. forall d. \list::List[_<n] (Qubit{d},Qubit{d}).
  let revStep = lift forall step.\(qs, q)::(List[_<step] (Qubit{d},Qubit{d}), (Qubit{d},Qubit{d})). qs:q
  in fold(revStep, [], list)
-- zips two lists of n qubits
qzip = forall n. forall d. \(a,b) :: (List[_<n+1] Qubit{d}, List[_<n+1] Qubit{d}).
  let qzipStep = lift forall step. \((llist,reslist),relem) :: ((List[_<(n+1)-step] Qubit{d}, List[_<step] (Qubit{d},Qubit{d})), Qubit{d}).
    let ls:lelem = (llist !:: List[_<(n-step)+1] Qubit{d}) in -- we can assume this because zipstep is only called by fold, where step < n+1
    let reslist = reslist : (lelem, relem) in
    (ls, reslist)
  in let (_, zippedlist) = (fold(qzipStep, (a,[]), b) :: (List[_<0] Qubit{d}, List[_<n+1] (Qubit{d},Qubit{d})))
  in zippedlist
-- unzips a list of i pairs of qubits
qunzip = forall n. forall d. \a :: List[_<n] (Qubit{d},Qubit{d}).
  let qunzipStep = lift forall step. \((leftList, rightList), pair) :: ((List[_<step] Qubit{d}, List[_<step] Qubit{d}), (Qubit{d},Qubit{d})).
    let (a,b) = pair in
    (leftList:a, rightList:b)
  in fold(qunzipStep, ([],[]), (force revpair @n @d) a)
-- 
interleave = forall n. forall d. \a :: List[_<n] (Qubit{d},Qubit{d}). 
  let interleaveStep = lift forall step . \(reslist, pair) :: (List[_<step] ((Qubit{d},Qubit{d}),Qubit{d}), (Qubit{d},Qubit{d})).
    let c = apply(QInit0,()) in reslist:(pair,c)
  in fold(interleaveStep, [], (force revpair @n @d) a)
--- CARRY ---
carry :: ![0](forall dc. forall da. forall db. forall dcnext. (Qubit{dc}, Qubit{da}, Qubit{db}, Qubit{dcnext}) -o[4, 0] (Qubit{max(1+dc,1+1+max(1+da,max(1+db,1+dcnext)))},Qubit{1+max(1+da,max(1+db,1+dcnext))},Qubit{max(1+dc,1+1+max(1+da,max(1+db,1+dcnext)))},Qubit{max(1+dc,1+1+max(1+da,max(1+db,1+dcnext)))}))
carry dc da db dcnext (c, a, b, cnext) =
  let (a, b, cnext) = (force toffoli @da @db @dcnext) a b cnext in
  let (a, b) = (force cnot @max(1+da,max(1+db,1+dcnext)) @max(1+da,max(1+db,1+dcnext))) a b in
  let (c, b, cnext) = (force toffoli @dc @1+max(1+da,max(1+db,1+dcnext)) @max(1+da,max(1+db,1+dcnext))) c b cnext in
  (c, a, b, cnext)
--- SUM ---
csum :: ![0](forall dc. forall da. forall db. (Qubit{dc}, Qubit{da}, Qubit{db}) -o[3, 0] (Qubit{max(1+dc,1+max(da+1,db+1))},Qubit{max(1+da,1+db)},Qubit{max(1+dc,1+max(da+1,db+1))}))
csum dc da db (c, a, b) =
  let (a,b) = (force cnot @da @db) a b in
  let (c, b) = (force cnot @dc @max(da+1,db+1)) c b in
  (c, a, b)
--- ADDER ---
adderFirstPhase :: ![0](forall[0,0] n. forall d. (List[_<n+1] Qubit{d}, List[_<n+1] Qubit{d}) -o[3*n + 4, 0] (List[_<n+1] ((Qubit{d}, Qubit{d}), Qubit{d}), Qubit{d}))
adderFirstPhase n d (a, b) =
  let ab = (force qzip @n @d) (a,b) in
  let abc = (force interleave @n+1 @d) ab in --every first position is a, every second is b, every third is c
  let initialAcc = apply(QInit0,()) in
  let adderStepFirst = lift forall step . \((reslist, c), ((a,b),cnext)) :: ((List[_<step] ((Qubit{d+step*3},Qubit{d+step*3}),Qubit{d+step*3}), Qubit{d+step*3}), ((Qubit{d+step*3},Qubit{d+step*3}),Qubit{d+step*3})) .
    let (c, a, b, cnext) = (force carry @d+step*3 @d+step*3 @d+step*3 @d+step*3) (c, a, b, cnext) in
    (reslist : ((c, a), b), cnext) in
  let (reslist,clast) = fold(adderStepFirst, ([], initialAcc), abc) in
  (reslist,clast) -- note that the output triples are reversed
-- 
adderSecondPhase :: ![0](forall[0,0] n. (Qubit{d}, List[_<n] ((Qubit{d}, Qubit{d}), Qubit{d})) -o[3*n + 1, 0] (Qubit{d}, List[_<n] (Qubit{d}, Qubit{d})))
adderSecondPhase n (cfirst, abc) =
  --subcircuit used in the second phase: carry followed by sum, carry discarded:
  let csum = lift \(c,a,b,cnext) :: (Qubit{d},Qubit{d},Qubit{d},Qubit{d}). 
      let (c,a,b,cnext) = force carry (c,a,b,cnext) in
      let (c,a,b) = force csum (c,a,b) in
      let _ = (force qdiscard) cnext in
      (c,a,b) in
  let boxedSum = box csum in
  --step function:
  let adderStepSecond = lift $ forall step. \((cnext, reslist),((c,a),b)) :: ((Qubit{d}, (List[_<step] (Qubit{d},Qubit{d}))),((Qubit{d},Qubit{d}),Qubit{d})).
    let (c,a,b) = apply(boxedSum, (c,a,b,cnext)) in
    (c, reslist:(a,b))
  in fold(adderStepSecond, (cfirst,[]), abc)
--whole adder circuit:
adder :: ![0](forall[0,0] n. (List[_<n+1] Qubit{d}, List[_<n+1] Qubit{d}) -o[3*(n+1) + 1, 0] (List[_<n+1] Qubit{d}, List[_<n+2] Qubit{d}))
adder n (a, b) =
  let (rest:((c,a),b),lsb) = (force adderFirstPhase @n) (a,b) in
  let (a,b) = force cnot a b in
  let (c,a,b) = force csum (c,a,b) in
  let (lastc, final) = (force adderSecondPhase @n) (c, rest) in
  let _ = (force qdiscard) lastc in
  let complete = (((force revpair @n) final) : (a,b)) in --add the first block back in
  let (ares,b) = (force qunzip @ n+1) complete in --separate a and b
  let bres = b : lsb in -- add the lsb back in b
  ((force rev @ n+1) ares, (force rev @ n+2) bres) --rearrange the bits in the right order
--- MAIN ---
-- init n qubits to |0>
-- qinit0Many = forall n. forall d.
--   let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{d}, ()). qs:(force qinit0)
--   in fold(qinitStep, [], force range @n+1)
-- main =
--   (force adder @1) (force qinit0Many @1,force qinit0Many @1)