main :: !(Qubit -o Qubit)
main = let id = (\f :: (Qubit -o Qubit) . f) 
    in id (\q :: Qubit . q)