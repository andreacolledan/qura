module Lang.Library.Prelude
where

import Index.AST
import Lang.Expr.AST
import Lang.Expr.Pattern
import Lang.Library.Constant
import Lang.Type.AST
import Circuit

--- PRELUDE MODULE -------------------------------------------------------------------------------------------
---
--- This module contains the definition of some library expressions which wrap the constants of the PQR language.
--- This is meant to simulate the standard library of PQR in the absence of an actual linking system.
-------------------------------------------------------------------------------------------------------------

--- BASIC GATES AND OPERATIONS --------------------------------------------------------------------------------

-- | @qinit0@ is the function that initializes a qubit to the |0⟩ state.
qinit0 :: Expr
qinit0 = ELift $ EAbs (PVar "_") TUnit $ EApply (EConst (Boxed QInit0)) EUnit

-- | @qinit1@ is the function that initializes a qubit to the |1⟩ state.
qinit1 :: Expr
qinit1 = ELift $ EAbs (PVar "_") TUnit $ EApply (EConst (Boxed QInit1)) EUnit

-- | @qdiscard@ is the function that discards a qubit.
qdiscard :: Expr
qdiscard = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed QDiscard)) (EVar "q"))

-- | @meas@ is the function that measures a qubit.
meas :: Expr
meas = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed Meas)) (EVar "q"))

-- | @cinit0@ is the expression that initializes a bit to the 0 state.
cinit0 :: Expr
cinit0 = ELift $ EAbs (PVar "_") TUnit $ EApply (EConst (Boxed CInit0)) EUnit

-- | @cinit1@ is the expression that initializes a bit to the 1 state.
cinit1 :: Expr
cinit1 = ELift $ EAbs (PVar "_") TUnit $ EApply (EConst (Boxed CInit1)) EUnit

-- | @cdiscard@ is the function that discards a bit.
cdiscard :: Expr
cdiscard = ELift $ EAbs (PVar "c") (TWire Bit) (EApply (EConst (Boxed CDiscard)) (EVar "c"))

-- | @hadamard@ is the function that applies the Hadamard gate to a qubit.
hadamard :: Expr
hadamard = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed Hadamard)) (EVar "q"))

-- | @pauliX@ is the function that applies the Pauli-X gate to a qubit.
pauliX :: Expr
pauliX = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed PauliX)) (EVar "q"))

-- | @pauliY@ is the function that applies the Pauli-Y gate to a qubit.
pauliY :: Expr
pauliY = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed PauliY)) (EVar "q"))

-- | @pauliZ@ is the function that applies the Pauli-Z gate to a qubit.
pauliZ :: Expr
pauliZ = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed PauliZ)) (EVar "q"))

-- | @tgate@ is the function that applies the T gate to a qubit.
tgate :: Expr
tgate = ELift $ EAbs (PVar "q") (TWire Qubit) (EApply (EConst (Boxed T)) (EVar "q"))

-- | @cnot@ is the function that applies the CNOT gate to a pair qubits.
cnot :: Expr
cnot = ELift $ EAbs (PVar "ctrl") (TWire Qubit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EConst (Boxed CNot)) (ETuple [EVar "ctrl", EVar "trgt"])

-- | @cz@ is the function that applies the CZ gate to a pair qubits.
cz :: Expr
cz = ELift $ EAbs (PVar "ctrl") (TWire Qubit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EConst (Boxed CZ)) (ETuple [EVar "ctrl", EVar "trgt"])

-- | @ccnot@ is the function that applies the CNOT gate to a pair qubits.
ccnot :: Expr
ccnot = ELift $ EAbs (PVar "ctrl") (TWire Bit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EConst (Boxed CCNot)) (ETuple [EVar "ctrl", EVar "trgt"])

-- | @ccz@ is the function that applies the CZ gate to a pair qubits.
ccz :: Expr
ccz = ELift $ EAbs (PVar "ctrl") (TWire Bit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EConst (Boxed CCZ)) (ETuple [EVar "ctrl", EVar "trgt"])

-- | @toffoli@ is the function that applies the Toffoli gate to three qubits.
toffoli :: Expr
toffoli = ELift $ EAbs (PVar "ctrl1") (TWire Qubit) $ EAbs (PVar "ctrl2") (TWire Qubit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EConst (Boxed Toffoli)) (ETuple [EVar "ctrl1", EVar "ctrl2", EVar "trgt"])

-- | @cR@ is the function that applies the controlled-R gate of angle 2π/2^n to a pair of qubits.
cR :: Expr
cR = ELift $ EIAbs "n" $ EAbs (PVar "ctrl") (TWire Qubit) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EIApp (EConst MakeCRGate) (IndexVariable "n")) (ETuple [EVar "ctrl", EVar "trgt"])

-- | @mcZ@ is the function that applies the multi-controlled Z gate to a list of control qubits and a target qubit.
mcZ :: Expr
mcZ = ELift $ EIAbs "n" $ EAbs (PVar "ctrls") (TList "i" (IndexVariable "n") (TWire Qubit)) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EIApp (EConst MakeNCZ) (IndexVariable "n")) (ETuple [EVar "ctrls", EVar "trgt"])

-- | @mcnot@ is the function that applies the multi-controlled NOT (Toffoli) gate to a list of control qubits and a target qubit.
mcnot :: Expr
mcnot = ELift $ EIAbs "n" $ EAbs (PVar "ctrls") (TList "i" (IndexVariable "n") (TWire Qubit)) $ EAbs (PVar "trgt") (TWire Qubit) $ EApply (EIApp (EConst MakeNToffoli) (IndexVariable "n")) (ETuple [EVar "ctrls", EVar "trgt"])

----- LIST FUNCTIONS --------------------------------------------------------------------------------------------
--
---- | @qrev@ is the function that reverses a list of qubits.
--qrev :: Expr
--qrev = ELift $ EIAbs "n" $ EAbs (PVar "qs") (TList (IndexVariable "n") (TWire Qubit)) $ EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "q"]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit]) (ECons (EVar "q") (EVar "acc"))) (ENil $ Just $ TWire Qubit) (EVar "qs")
--
---- | @crev@ is the function that reverses a list of bits.
--crev :: Expr
--crev = ELift $ EIAbs "n" $ EAbs (PVar "cs") (TList (IndexVariable "n") (TWire Bit)) $ EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "c"]) (TTensor [TList (IndexVariable "i") (TWire Bit), TWire Bit]) (ECons (EVar "c") (EVar "acc"))) (ENil $ Just $ TWire Bit) (EVar "cs")
--
---- | @qmap@ is the function that maps a signle-qubit circuit-building function over a list of qubits.
--qmap :: Expr
--qmap = ELift $
--  EIAbs "n" $ EAbs (PVar "f") (TBang $ TArrow (TWire Qubit) (TWire Qubit) (IndexVariable "n") (Number 0)) $
--  EIAbs "m" $ EAbs (PVar "list") (TList (IndexVariable "m") (TWire Qubit)) $
--    EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "q"]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TWire Qubit])  (ECons (EApp (EForce $ EVar "f") (EVar "q")) (EVar "acc")))
--    (ENil $ Just $ TWire Qubit)
--    (EApp (EIApp (EForce $ EVar "qrev") (IndexVariable "m")) (EVar "list"))
--
---- | @cmap@ is the function that maps a signle-qubit circuit-building function over a list of bits.
--cmap :: Expr
--cmap = ELift $
--  EIAbs "n" $ EAbs (PVar "f") (TBang $ TArrow (TWire Bit) (TWire Bit) (IndexVariable "n") (Number 0)) $
--  EIAbs "m" $ EAbs (PVar "list") (TList (IndexVariable "m") (TWire Bit)) $
--    EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "c"]) (TTensor [TList (IndexVariable "i") (TWire Bit), TWire Bit]) (ECons (EApp (EForce $ EVar "f") (EVar "c")) (EVar "acc"))) (ENil $ Just $ TWire Bit) (EApp (EIApp (EForce $ EVar "crev") (IndexVariable "m")) (EVar "list"))
--
---- | @qconcat@ is the function that concatenates two lists of qubits.
--qconcat :: Expr
--qconcat = ELift $ -- lift
--  EIAbs "n" $ EAbs (PVar "list1") (TList (IndexVariable "n") (TWire Qubit)) $ -- @i. \list1 :: List[n] Qubit.
--  EIAbs "m" $ EAbs (PVar "list2") (TList (IndexVariable "m") (TWire Qubit)) $ -- @j. \list2 :: List[m] Qubit.
--    EFold -- fold (
--    (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "qs", PVar "q"]) (TTensor [TList (Plus (IndexVariable "i") (IndexVariable "m")) (TWire Qubit), TWire Qubit]) -- @k. \(qs,q) :: (List[i+m] Qubit, Qubit).
--      (ECons (EVar "q") (EVar "qs"))) -- (q:qs),
--    (EVar "list2")  -- list2,
--    (EApp (EIApp (EForce $ EVar "qrev") (IndexVariable "n")) (EVar "list1")) -- (force qrev @ n) list1)
--
---- | @cconcat@ is the function that concatenates two lists of bits.
--cconcat :: Expr
--cconcat = ELift $ -- lift
--  EIAbs "n" $ EAbs (PVar "list1") (TList (IndexVariable "n") (TWire Bit)) $ -- @i. \list1 :: List[n] Bit.
--  EIAbs "m" $ EAbs (PVar "list2") (TList (IndexVariable "m") (TWire Bit)) $ -- @j. \list2 :: List[m] Bit.
--    EFold -- fold (
--    (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "qs", PVar "q"]) (TTensor [TList (Plus (IndexVariable "i") (IndexVariable "m")) (TWire Bit), TWire Bit]) -- @k. \(qs,q) :: (List[i+m] Bit, Bit).
--      (ECons (EVar "q") (EVar "qs"))) -- (q:qs),
--    (EVar "list2")  -- list2,
--    (EApp (EIApp (EForce $ EVar "crev") (IndexVariable "n")) (EVar "list1")) -- (force qrev @ n) list1)
--
--
----- MULTI-QUBIT GATES -----------------------------------------------------------------------------------------
--
---- | @mqinit0@ is the function that initializes a list of qubits to the |0⟩ state.
--mQinit0 :: Expr
--mQinit0 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TUnit])
--    (ECons (EApply (EConst QInit0) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IndexVariable "n"))
--
---- | @mqinit1@ is the function that initializes a list of qubits to the |1⟩ state.
--mQinit1 :: Expr
--mQinit1 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IndexVariable "i") (TWire Qubit), TUnit])
--    (ECons (EApply (EConst QInit1) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IndexVariable "n"))
--
---- | @mcinit0@ is the function that initializes a list of bits to the 0 state.
--mCinit0 :: Expr
--mCinit0 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IndexVariable "i") (TWire Bit), TUnit])
--    (ECons (EApply (EConst CInit0) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IndexVariable "n"))
--
---- | @mcinit1@ is the function that initializes a list of bits to the 1 state.
--mCinit1 :: Expr
--mCinit1 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IndexVariable "i") (TWire Bit), TUnit])
--    (ECons (EApply (EConst CInit1) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IndexVariable "n"))
--
---- | @mHadamard@ is the function that applies the Hadamard gate to a list of qubits.
--mHadamard :: Expr
--mHadamard = ELift $ EApp (EIApp (EForce (EVar "qmap")) (Number 1)) hadamard
--
---- | @mX@ is the function that applies the Pauli-X gate to a list of qubits.
--mX :: Expr
--mX = ELift $ EApp (EIApp (EForce (EVar "qmap")) (Number 1)) pauliX

--- MISC -----------------------------------------------------------------------------------------------------

-- | @range@ is the expression that generates a list of unit of length n. Used to encode natural numbers to iterate.
range :: Expr
range = EIAbs "n" $ EIApp (EConst MakeUnitList) (IndexVariable "n")

--- Library definition ---------------------------------------------------------------------------------------

libraryBindings :: [(String, Expr)]
libraryBindings = [
  ("qinit0", qinit0),
  ("qinit1", qinit1),
  ("qdiscard", qdiscard),
  ("meas", meas),
  ("cinit0", cinit0),
  ("cinit1", cinit1),
  ("cdiscard", cdiscard),
  ("hadamard", hadamard),
  ("pauliX", pauliX),
  ("pauliY", pauliY),
  ("pauliZ", pauliZ),
  ("tgate", tgate),
  ("cnot", cnot),
  ("cz", cz),
  ("ccnot", ccnot),
  ("ccz", ccz),
  ("toffoli", toffoli),
  ("cR", cR),
  ("mcZ", mcZ),
  ("range", range),
  ("mcnot", mcnot){-,
  ("qrev", qrev),
  ("crev", crev),
  ("qmap", qmap),
  ("cmap", cmap),
  ("mHadamard", mHadamard),
  ("mX", mX),
  ("mQinit0", mQinit0),
  ("mQinit1", mQinit1),
  ("mCinit0", mCinit0),
  ("mCinit1", mCinit1),
  ("qconcat", qconcat),
  ("cconcat", cconcat)-}
  ]

library :: Expr -> Expr
library e = foldr (\(name, def) acc -> ELet (PVar name) def acc) e libraryBindings
