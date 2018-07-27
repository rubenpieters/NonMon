module Language.Syntax where

type DeclarationIdentifier = String
type VariableIdentifier = (String, Int)
type OperationIdentifier = String
data InjectIdentifier = Inj0 | Inj1
  deriving (Show, Eq, Ord)

type Program = [TopLevelDeclaration]

data TopLevelDeclaration
  = TLDeclaration DeclarationIdentifier Value
  deriving (Show, Eq, Ord)

-- V,W ::=
data Value
  -- x
  = ValVariable VariableIdentifier
  -- ()
  | ValUnit
  -- (V, W)
  | ValPair Value Value
  -- in_i V
  | ValInjection InjectIdentifier Value
  -- {M}
  | ValThunk Computation
  deriving (Show, Eq, Ord)

-- M,N ::=
data Computation
  -- split(V, M)
  = ComSplit Value Computation
  -- case0(V)
  | ComCase0 Value
  -- case(V, M, N)
  | ComCase Value Computation Computation
  -- V!
  | ComForce Value
  -- return V
  | ComReturn Value
  -- let x <- M in N
  | ComLet VariableIdentifier Computation Computation
  -- \x.M
  | ComLambda VariableIdentifier Computation
  -- M V
  | ComLambdaApply Computation Value
  -- symbol ':' introduced for operations for easier parsing
  -- :op V
  | ComOperationApply OperationIdentifier Value
  -- handle M with H
  | ComHandle Computation Handler
  deriving (Show, Eq, Ord)

type Handler = [HandlerClause]

-- H ::=
data HandlerClause
  -- | return x -> M
  = HanValClause VariableIdentifier Computation
  -- | :op p k -> M
  | HanOpClause OperationIdentifier VariableIdentifier VariableIdentifier Computation
  deriving (Show, Eq, Ord)
