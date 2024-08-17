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
qinit0 = ELift $ EApply (EConst (Boxed (QInit False))) EUnit

-- | @qinit1@ is the function that initializes a qubit to the |1⟩ state.
qinit1 :: Expr
qinit1 = ELift $ EApply (EConst (Boxed (QInit True))) EUnit

-- | @qdiscard@ is the function that discards a qubit.
qdiscard :: Expr
qdiscard = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed QDiscard) `EIApp` IVar "l") (EVar "q"))

-- | @meas@ is the function that measures a qubit.
meas :: Expr
meas = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed Meas) `EIApp` IVar "l") (EVar "q"))

-- | @cinit0@ is the expression that initializes a bit to the 0 state.
cinit0 :: Expr
cinit0 = ELift $ EApply (EConst (Boxed (CInit False))) EUnit

-- | @cinit1@ is the expression that initializes a bit to the 1 state.
cinit1 :: Expr
cinit1 = ELift $ EApply (EConst (Boxed (CInit True))) EUnit

-- | @cdiscard@ is the function that discards a bit.
cdiscard :: Expr
cdiscard = ELift $ EIAbs "l" $ EAbs (PVar "c") (TWire Bit (Just $ IVar "l")) (EApply (EConst (Boxed CDiscard) `EIApp` IVar "l") (EVar "c"))

-- | @hadamard@ is the function that applies the Hadamard gate to a qubit.
hadamard :: Expr
hadamard = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed Hadamard) `EIApp` IVar "l") (EVar "q"))

-- | @pauliX@ is the function that applies the Pauli-X gate to a qubit.
pauliX :: Expr
pauliX = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed PauliX) `EIApp` IVar "l") (EVar "q"))

-- | @pauliY@ is the function that applies the Pauli-Y gate to a qubit.
pauliY :: Expr
pauliY = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed PauliY) `EIApp` IVar "l") (EVar "q"))

-- | @pauliZ@ is the function that applies the Pauli-Z gate to a qubit.
pauliZ :: Expr
pauliZ = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed PauliZ) `EIApp` IVar "l") (EVar "q"))

-- | @tgate@ is the function that applies the T gate to a qubit.
tgate :: Expr
tgate = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed T) `EIApp` IVar "l") (EVar "q"))

-- | @cnot@ is the function that applies the CNOT gate to a pair qubits.
cnot :: Expr
cnot = ELift $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Qubit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst (Boxed CNot) `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])

-- | @cz@ is the function that applies the CZ gate to a pair qubits.
cz :: Expr
cz = ELift $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Qubit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst (Boxed CZ) `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])

-- | @ccnot@ is the function that applies the CNOT gate to a pair qubits.
ccnot :: Expr
ccnot = ELift $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Bit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst (Boxed CCNot) `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])

-- | @ccz@ is the function that applies the CZ gate to a pair qubits.
ccz :: Expr
ccz = ELift $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Bit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst (Boxed CCZ) `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])

-- | @toffoli@ is the function that applies the Toffoli gate to three qubits.
toffoli :: Expr
toffoli = ELift $ EIAbs "lctrl1" $ EIAbs "lctrl2" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl1") (TWire Qubit (Just $ IVar "lctrl1")) $ EAbs (PVar "ctrl2") (TWire Qubit (Just $ IVar "lctrl2")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst (Boxed Toffoli) `EIApp` IVar "lctrl1" `EIApp` IVar "lctrl2" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl1", EVar "ctrl2", EVar "trgt"])

-- | @cR@ is the function that returns the controlled-R gate of angle 2π/2^n to a pair of qubits.
cR :: Expr
cR = ELift $ EIAbs "n" $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Qubit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst MakeCRGate `EIApp` IVar "n" `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])

----- LIST FUNCTIONS --------------------------------------------------------------------------------------------
--
---- | @qrev@ is the function that reverses a list of qubits.
--qrev :: Expr
--qrev = ELift $ EIAbs "n" $ EAbs (PVar "qs") (TList (IVar "n") (TWire Qubit)) $ EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "q"]) (TTensor [TList (IVar "i") (TWire Qubit), TWire Qubit]) (ECons (EVar "q") (EVar "acc"))) (ENil $ Just $ TWire Qubit) (EVar "qs")
--
---- | @crev@ is the function that reverses a list of bits.
--crev :: Expr
--crev = ELift $ EIAbs "n" $ EAbs (PVar "cs") (TList (IVar "n") (TWire Bit)) $ EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "c"]) (TTensor [TList (IVar "i") (TWire Bit), TWire Bit]) (ECons (EVar "c") (EVar "acc"))) (ENil $ Just $ TWire Bit) (EVar "cs")
--
---- | @qmap@ is the function that maps a signle-qubit circuit-building function over a list of qubits.
--qmap :: Expr
--qmap = ELift $
--  EIAbs "n" $ EAbs (PVar "f") (TBang $ TArrow (TWire Qubit) (TWire Qubit) (IVar "n") (Number 0)) $
--  EIAbs "m" $ EAbs (PVar "list") (TList (IVar "m") (TWire Qubit)) $
--    EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "q"]) (TTensor [TList (IVar "i") (TWire Qubit), TWire Qubit])  (ECons (EApp (EForce $ EVar "f") (EVar "q")) (EVar "acc")))
--    (ENil $ Just $ TWire Qubit)
--    (EApp (EIApp (EForce $ EVar "qrev") (IVar "m")) (EVar "list"))
--
---- | @cmap@ is the function that maps a signle-qubit circuit-building function over a list of bits.
--cmap :: Expr
--cmap = ELift $
--  EIAbs "n" $ EAbs (PVar "f") (TBang $ TArrow (TWire Bit) (TWire Bit) (IVar "n") (Number 0)) $
--  EIAbs "m" $ EAbs (PVar "list") (TList (IVar "m") (TWire Bit)) $
--    EFold (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "c"]) (TTensor [TList (IVar "i") (TWire Bit), TWire Bit]) (ECons (EApp (EForce $ EVar "f") (EVar "c")) (EVar "acc"))) (ENil $ Just $ TWire Bit) (EApp (EIApp (EForce $ EVar "crev") (IVar "m")) (EVar "list"))
--
---- | @qconcat@ is the function that concatenates two lists of qubits.
--qconcat :: Expr
--qconcat = ELift $ -- lift
--  EIAbs "n" $ EAbs (PVar "list1") (TList (IVar "n") (TWire Qubit)) $ -- @i. \list1 :: List[n] Qubit.
--  EIAbs "m" $ EAbs (PVar "list2") (TList (IVar "m") (TWire Qubit)) $ -- @j. \list2 :: List[m] Qubit.
--    EFold -- fold (
--    (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "qs", PVar "q"]) (TTensor [TList (Plus (IVar "i") (IVar "m")) (TWire Qubit), TWire Qubit]) -- @k. \(qs,q) :: (List[i+m] Qubit, Qubit).
--      (ECons (EVar "q") (EVar "qs"))) -- (q:qs),
--    (EVar "list2")  -- list2,
--    (EApp (EIApp (EForce $ EVar "qrev") (IVar "n")) (EVar "list1")) -- (force qrev @ n) list1)
--
---- | @cconcat@ is the function that concatenates two lists of bits.
--cconcat :: Expr
--cconcat = ELift $ -- lift
--  EIAbs "n" $ EAbs (PVar "list1") (TList (IVar "n") (TWire Bit)) $ -- @i. \list1 :: List[n] Bit.
--  EIAbs "m" $ EAbs (PVar "list2") (TList (IVar "m") (TWire Bit)) $ -- @j. \list2 :: List[m] Bit.
--    EFold -- fold (
--    (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "qs", PVar "q"]) (TTensor [TList (Plus (IVar "i") (IVar "m")) (TWire Bit), TWire Bit]) -- @k. \(qs,q) :: (List[i+m] Bit, Bit).
--      (ECons (EVar "q") (EVar "qs"))) -- (q:qs),
--    (EVar "list2")  -- list2,
--    (EApp (EIApp (EForce $ EVar "crev") (IVar "n")) (EVar "list1")) -- (force qrev @ n) list1)
--
--
----- MULTI-QUBIT GATES -----------------------------------------------------------------------------------------
--
---- | @mqinit0@ is the function that initializes a list of qubits to the |0⟩ state.
--mQinit0 :: Expr
--mQinit0 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IVar "i") (TWire Qubit), TUnit])
--    (ECons (EApply (EConst QInit0) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IVar "n"))
--
---- | @mqinit1@ is the function that initializes a list of qubits to the |1⟩ state.
--mQinit1 :: Expr
--mQinit1 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IVar "i") (TWire Qubit), TUnit])
--    (ECons (EApply (EConst QInit1) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IVar "n"))
--
---- | @mcinit0@ is the function that initializes a list of bits to the 0 state.
--mCinit0 :: Expr
--mCinit0 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IVar "i") (TWire Bit), TUnit])
--    (ECons (EApply (EConst CInit0) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IVar "n"))
--
---- | @mcinit1@ is the function that initializes a list of bits to the 1 state.
--mCinit1 :: Expr
--mCinit1 = ELift $ EIAbs "n" $ EFold
--  (ELift $ EIAbs "i" $ EAbs (PTuple [PVar "acc", PVar "_"]) (TTensor [TList (IVar "i") (TWire Bit), TUnit])
--    (ECons (EApply (EConst CInit1) EUnit) (EVar "acc")))
--  (ENil $ Just TUnit)
--  (EIApp (EConst MakeUnitList) (IVar "n"))
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
range = EIAbs "n" $ EIApp (EConst MakeUnitList) (IVar "n")

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
  ("range", range){-,
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
