test :: !(() -o ((),()))
test a = let a = ((),()) in a

test2 = \a::(). let a = ((),()) in a