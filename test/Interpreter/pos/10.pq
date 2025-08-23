hadamardLayer :: !(forall n. List[_<n] Qubit -o List[_<n] Qubit)
hadamardLayer n qs =
  let hadamardLayerStep = lift forall step. \(outputs, input) :: (List[_<step] Qubit, Qubit) .
        let output = apply(Hadamard @0, input) in
        outputs : output
  in fold(hadamardLayerStep, [], qs)

initRegister0 :: !(forall n. List[_<n] Qubit)
initRegister0 n =
  let init0Step = lift forall step. \(newQubits, _) :: (List[_<step] Qubit, ()) .
        let newQubit = apply(QInit0, ()) in
        newQubits : newQubit
  in fold(init0Step, [], MakeUnitList @ n)

main :: !(List[_<15] Qubit)
main = let qs = force initRegister0 @15 in
        (force hadamardLayer @15) qs