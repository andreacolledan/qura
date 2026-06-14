main :: !(Qubit -o Qubit)
main = let id = (lift $ \f :: (Qubit -o Qubit) . f) 
    in let (id1, id2) = (force id, force id)
        in id1 (id2  (\q :: Qubit . q))