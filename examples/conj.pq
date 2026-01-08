-- The oracle conj : {0,1}^3 -> {0,1} such that conj(x) = 1 <==> x = 111
conj = forall d.
  box $ lift \(reg, a) :: (List[_<3] Qubit{d}, Qubit{d}).
    (force mcnot @ 3 @ d @ d) reg a -- Ancilla is flipped iff reg is 111


main = 
