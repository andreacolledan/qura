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

-- | @qnot@ is the function that applies the Pauli-X gate to a qubit.
qnot :: Expr
qnot = ELift $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) (EApply (EConst (Boxed PauliX) `EIApp` IVar "l") (EVar "q"))

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

-- | @mcnot@ is the function that applies the multi-controlled NOT gate to a list of control qubits and a target qubit.
mcnot :: Expr
mcnot = ELift $ EIAbs "n" $ EIAbs "lctrls" $ EIAbs "ltrgt" $ EAbs (PVar "ctrls") (TList "_" (IVar "n") (TWire Qubit (Just $ IVar "lctrls"))) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst MakeMCNot `EIApp` IVar "n" `EIApp` IVar "lctrls" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrls", EVar "trgt"])

-- | @rgate@ is the function that returns the R gate of angle 2π/2^n to a qubit.
rgate :: Expr
rgate = ELift $ EIAbs "n" $ EIAbs "l" $ EAbs (PVar "q") (TWire Qubit (Just $ IVar "l")) $ EApply (EConst MakeRGate `EIApp` IVar "n" `EIApp` IVar "l") (EVar "q")

-- | @cr@ is the function that returns the controlled-R gate of angle 2π/2^n to a pair of qubits.
cr :: Expr
cr = ELift $ EIAbs "n" $ EIAbs "lctrl" $ EIAbs "ltrgt" $ EAbs (PVar "ctrl") (TWire Qubit (Just $ IVar "lctrl")) $ EAbs (PVar "trgt") (TWire Qubit (Just $ IVar "ltrgt")) $ EApply (EConst MakeCRGate `EIApp` IVar "n" `EIApp` IVar "lctrl" `EIApp` IVar "ltrgt") (ETuple [EVar "ctrl", EVar "trgt"])


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
  ("qnot", qnot),
  ("pauliY", pauliY),
  ("pauliZ", pauliZ),
  ("tgate", tgate),
  ("cnot", cnot),
  ("cz", cz),
  ("ccnot", ccnot),
  ("ccz", ccz),
  ("toffoli", toffoli),
  ("mcnot", mcnot),
  ("rgate", rgate),
  ("cr", cr),
  ("range", range)
  ]

library :: Expr -> Expr
library e = foldr (\(name, def) acc -> ELet (PVar name) def acc) e libraryBindings
