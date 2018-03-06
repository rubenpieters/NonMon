module Syntax where

type VariableIdentifier = String
type OperationIdentifier = String
type InjectIdentifier = Int

-- A,B ::=
data ValueType
  -- 1
  = VTUnit
  -- A x B
  | VTProduct ValueType ValueType
  -- 0
  | VTEmpty
  -- A + B
  | VTSum ValueType ValueType
  -- {C}_E
  | VTThunk ComputationType EffectSignature

-- C ::=
data ComputationType
  -- [A]
  = CTReturner
  -- A -> C
  | CTFunction

-- E ::= op_0: A_0 -> B_0, ..., op_n: A_n -> B_n
type EffectSignature = [(OperationIdentifier, ValueType, ValueType)]

-- Γ ::= x_0: A_0, ..., x_n: A_n
type Environment = [(VariableIdentifier, ValueType)]

-- V,W ::=
data Value
  -- x
  = ValVariable VariableIdentifier
  -- ()
  | ValUnit
  -- (V, W)
  | ValPair Value Value
  -- inj_i V
  | ValInjection InjectIdentifier Value
  -- {M}
  | ValThunk Computation

-- M,N ::=
data Computation
  -- split(V, x.y.M)
  = ComSplit Value Computation
  -- case0(V)
  | ComCase0 Value
  -- case(V, x.M, y.N)
  | ComCase Value Computation
  -- V!
  | ComForce Value
  -- return V
  | ComReturn Value
  -- let x <- M in N
  | ComLet VariableIdentifier Computation Computation
  -- \x.M
  | ComLambda VariableIdentifier Computation
  -- M V
  | ComApply Computation Value
  -- op V
  | ComOperationApply OperationIdentifier Value
