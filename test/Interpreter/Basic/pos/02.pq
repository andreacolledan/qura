main :: !(Qubit -o Qubit)
main = (\f :: (Qubit -o Qubit) . f) (\q :: Qubit . q)