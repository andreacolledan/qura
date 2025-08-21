-- negate a qubit q at depth d in a dumb way,
-- by initializing an ancillary qubit a to 1 and
-- applying a cnot with target q and control a
dumbNot :: ![0](forall[0,0] d. Qubit{d} -o[2,0] Qubit{d+1})
dumbNot d q =
  let a = force qinit1 in
  let (a,q) = (force cnot @0 @d) a q in
  let _ = (force qdiscard @(d+1)) a in
  q

-- negate twice a qubit y at depth x, using dumbNot
-- this is useless, but highlights qubit reuse
doubleNegation :: ![0](forall[0,0] x. Qubit{x} -o[2,0] Qubit{x+2})
doubleNegation x y =
  (force dumbNot @(x+1)) ((force dumbNot @x) y)

-- main :: !Qubit
main = 
  let k = apply(QInit1, ()) in
  -- (force dumbNot @0) k
  (force doubleNegation @0) k

-- (force dumbNot @(x+1)) ((force dumbNot @x) y)
-- (EApp 
--   (EIApp 
--     (EForce 
--       (EVar "dumbNot")
--     ) 
--     (Plus (IVar "x") (Number 1))
--   ) 
--   (EApp 
--     (EIApp 
--       (EForce 
--         (EVar "dumbNot")
--       ) 
--       (IVar "x")
--     ) 
--     (EVar "y")
--   )
-- )
-- (force cnot @0 @d) a q in 
-- (EApp 
--   (EApp 
--     (EIApp 
--       (EIApp 
--         (EForce 
--           (EVar "cnot")
--         ) 
--         (Number 0)
--       ) 
--       (IVar "d")
--     ) 
--     (EVar "a")
--   )
--   (EVar "q")
-- )

-- cnot
-- forall lctrl . (forall ltrgt . (\ctrl :: Qubit{lctrl} . (\trgt :: Qubit{ltrgt} . apply(((CNot @ lctrl) @ ltrgt), (ctrl, trgt)))))
-- (EIAbs 
--   "lctrl" 
--   (EIAbs 
--     "ltrgt" 
--     (EAbs 
--       (PVar "ctrl") 
--       (TWire 
--         Qubit 
--         (Just 
--           (IVar "lctrl")
--         )
--       ) 
--       (EAbs 
--         (PVar "trgt")
--         (TWire 
--           Qubit 
--           (Just 
--             (IVar "ltrgt")
--           )
--         )
--         (EApply 
--           (EIApp 
--             (EIApp 
--               (EConst 
--                 (Boxed CNot)
--               ) 
--               (IVar "lctrl")
--             ) 
--             (IVar "ltrgt")
--           )
--           (ETuple [EVar "ctrl", EVar "trgt"])
--         )
--       )
--     )
--   )
-- )