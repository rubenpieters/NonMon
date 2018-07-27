module Language.Typing where

import Language.Syntax

import Data.List

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
  deriving (Show, Eq, Ord)

-- C ::=
data ComputationType
  -- [A]
  = CTReturner ValueType
  -- A -> C
  | CTFunction ValueType ValueType
  deriving (Show, Eq, Ord)

-- E ::= { op: A -> B } ⨄ E | {f} ⨄ E | ∅
type EffectSignature =
  [(OperationIdentifier, ValueType, ValueType)]

type Environment = [EnvVariable]
type EnvVariable = (VariableIdentifier, Flow, ValueType)

data Flow = I | A | C | AC | M
  deriving (Show, Eq)

leq :: Flow -> Flow -> Bool
leq _ M = True
leq M _ = False
leq _ AC = True
leq AC _ = False
leq A C = False
leq C A = False
leq _ A = True
leq A _ = False
leq _ C = True
leq C _ = False
leq I I = True

geq :: Flow -> Flow -> Bool
geq a b = b `leq` a

typecheckA :: Environment -> Value -> Flow -> Either String ValueType
typecheckA env (ValVariable x) f = case findEnv env x f of
  Just (_, _, ty) -> Right ty
  Nothing -> Left ("cannot find variable " ++ show x ++ " for flow " ++ show f)
typecheckA env (ValUnit) f = Right VTUnit

typecheckM :: Environment -> Computation -> Either String ComputationType
typecheckM env (ComReturn v) = do
  a <- typecheckA env v I
  return (CTReturner a)
typecheckM env (ComLet x m n) = do
  ma <- typecheckM env m
  case ma of
    CTReturner a -> do
      -- TODO: infer or ascribe the flow for let
      c <- typecheckM ((x, I, a) : env) n
      return c
    _ -> Left "m not a computation"
typecheckM env (ComOperationApply op v) = do
  a' <- typecheckA env v A
  _ <- check a a'
  return (CTReturner b)
  where
    (a, b) = findOpType op
  

check :: ValueType -> ValueType -> Either String ()
check a b = if a == b
    then Right ()
    else Left ("type " ++ show a ++ " is not " ++ show b)

findEnv :: Environment -> VariableIdentifier -> Flow -> Maybe EnvVariable
findEnv env i f = find (\(i', f', _) -> i == i' && f `geq` f') env

findOpType :: OperationIdentifier -> (ValueType, ValueType)
findOpType "test" = (VTUnit, VTUnit)
findOpType op = error ("unknown op " ++ op)

test1 = ComLet ("x", 0) (ComOperationApply "test" ValUnit) (ComReturn (ValVariable ("x", 0)))
