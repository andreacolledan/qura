qinit0Many = forall n.
  let qinitStep = lift forall step. \(qs,u) :: (List[_<step] Qubit{0}, ()). qs:(force qinit0)
  in fold(qinitStep, [], force range @n)

main = -- two different ways of using MCNot
    let trgt = force qinit1 in
    let ctrls = force qinit0Many @ 3 in
    let (c,t) = apply((MakeMCNot @3 @0 @0), (ctrls, trgt)) in
    let t2 = (force qnot @0) t in
    (force mcnot @3 @0 @0) c t2