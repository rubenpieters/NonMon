{-# LANGUAGE LambdaCase #-}

module Language.Evaluate where

import Data.Function ((&))

import Language.Syntax
import Language.Printer

import System.IO.Unsafe

findDef :: Program -> DeclarationIdentifier -> Value
findDef [] def = error ("can't find top-level definition " ++ def)
findDef (TLDeclaration declName val : _) def | declName == def = val
findDef (_ : r) def = findDef r def

data OpType
  = InternalOp String
  | UserOp Computation

findOp :: Handler -> OperationIdentifier -> OpType
findOp [] "print" = InternalOp "print"
findOp [] op = error ("unhandled operation: " ++ op)
findOp (HanOpClause op' p k m : _) op | op' == op = UserOp $
  ComLambda p (ComLambda k m)
findOp (_ : r) op = findOp r op

findVal :: Handler -> Computation
findVal [] = error ("unhandled value clause")
findVal (HanValClause x m : _) = ComLambda x m
findVal (_ : r) = findVal r

repeatEval :: Program -> Computation -> IO Computation
repeatEval p com = do
  evalCom p com >>= \case
    Just newCom ->
      repeatEval p newCom
    Nothing -> return com

evalCom :: Program -> Computation -> IO (Maybe Computation)
-- split((v,w), \x.\y.m)
-- m[x=v,y=w]
evalCom p (ComSplit (ValPair v w) (ComLambda x (ComLambda y m))) =
  return $ return $
  m & substituteCom x v
    & substituteCom y w
evalCom p (ComSplit (ValPair v w) (ComLambda x _)) = error "runtime error: ComSplit, no \\x.\\y."
evalCom p (ComSplit (ValPair v w) _) = error "runtime error: ComSplit, no \\x."
evalCom p (ComSplit (ValVariable (v, _)) m) =
  let v' = findDef p v in
  return $ return $
  ComSplit v' m
evalCom p (ComSplit _ _) = error "runtime error: ComSplit, not splitting a pair"
--evalCom (ComCase0 ()) =
-- case(in_0 v, \x.m, \y.n)
-- m[x=v]
evalCom p (ComCase (ValInjection Inj0 v) (ComLambda x m) _) =
  return $ return $
  m & substituteCom x v
-- case(in_1 v, \x.m, \y.n)
-- n[y=v]
evalCom p (ComCase (ValInjection Inj1 v) (ComLambda y n) _) =
  return $ return $
  n & substituteCom y v
-- {m}!
-- m
evalCom p (ComForce (ValThunk m)) = return $ return m
evalCom p (ComForce (ValVariable (v, _))) =
  let v' = findDef p v in
  return $ return $
  ComForce v'
-- handle (return v) with h{| return x -> m, ...}
-- handle ((\x.m) v) with h
evalCom p (ComHandle (ComReturn v) h) =
  let
    m = findVal h
  in
  return $ return $
  ComLambdaApply m v
-- handle (let x <- :op v in n) with h{| :op p k -> m, ...}
-- if user operation
-- (\p.\k.m) v {\x. handle n with h}
-- if internal operation
-- InternalOp :op v (\x. handle n with h)
evalCom p (ComHandle (ComLet x@(_, t) (ComOperationApply op v) n) h) =
  let
    eM = findOp h op
    tickedHandle = (ComHandle n (h & addTickHan (t + 1)))
  in
  case eM of
    UserOp m ->
      return $ return $
      ComLambdaApply (ComLambdaApply m v) (ValThunk (ComLambda x tickedHandle))
    InternalOp op -> evalInternalOp op v (ComLambdaApply (ComLambda x tickedHandle))

evalCom p (ComHandle (ComLet x m n) h) = do
  m' <- repeatEval p m
  return $ return $
    ComHandle (ComLet x m' n) h
evalCom p (ComHandle m h) = do
  m' <- repeatEval p m
  return $ return $
    ComHandle m' h
-- let x <- (return V) in n
-- n[x=v]
evalCom p (ComLet x (ComReturn v) n) =
  return $ return $
  n & substituteCom x v
evalCom p (ComLet x (ComOperationApply op v) n) =
  return Nothing
evalCom p (ComLet x m n) = do
  m' <- repeatEval p m
  return $ return $
    ComLet x m' n
-- (\x.m) v
-- m[x=v]
evalCom p (ComLambdaApply (ComLambda x m) v) =
  return $ return $
  m & substituteCom x v
evalCom p (ComLambdaApply m v) = do
  m' <- repeatEval p m
  return $ return $
    ComLambdaApply m' v
evalCom p (ComLambda{}) = return Nothing
evalCom p (ComReturn{}) = return Nothing
evalCom _ com = error ("TODO: " ++ show com)

-- internal operations

evalInternalOp ::
  String -> Value -> (Value -> Computation) -> IO (Maybe Computation)
evalInternalOp "print" v k = do
      print ("PRINT: " ++ prettyVal v)
      return $ return (k ValUnit)
evalInternalOp op v k = error "unimplemented internal operation"

-- variable substitution

substituteCom :: VariableIdentifier -> Value -> Computation -> Computation
substituteCom x v (ComSplit v' m') =
  ComSplit
    (v' & substituteVal x v)
    (m' & substituteCom x v)
substituteCom x v (ComCase0 v') =
  ComCase0
    (v' & substituteVal x v)
substituteCom x v (ComCase v' m' n') =
  ComCase
    (v' & substituteVal x v)
    (m' & substituteCom x v)
    (n' & substituteCom x v)
substituteCom x v (ComForce v') =
  ComForce
    (v' & substituteVal x v)
substituteCom x v (ComReturn v') =
  ComReturn
    (v' & substituteVal x v)
substituteCom x v (ComLet x' m' n') =
  ComLet x'
    (m' & substituteCom x vTicked)
    (n' & substituteCom x vTicked)
  where
    mx = maxTickCom m'
    vTicked = v & addTickVal (mx + 1)
substituteCom x v (ComLambda x' m') =
  ComLambda x'
    (m' & substituteCom x vTicked)
  where
    mx = maxTickCom m'
    vTicked = v & addTickVal (mx + 1)
substituteCom x v (ComLambdaApply m' v') =
  ComLambdaApply
    (m' & substituteCom x v)
    (v' & substituteVal x v)
substituteCom x v (ComOperationApply op' v') =
  ComOperationApply op'
    (v' & substituteVal x v)
substituteCom x v (ComHandle m' h') =
  ComHandle
    (m' & substituteCom x v)
    (h' & substituteHan x v)

substituteHan :: VariableIdentifier -> Value -> Handler -> Handler
substituteHan _ _ [] = []
substituteHan x v (HanValClause x' m' : r) =
  HanValClause x'
    (m' & substituteCom x vTicked)
  : (r & substituteHan x v)
  where
    mx = maxTickCom m'
    vTicked = v & addTickVal (mx + 1)
substituteHan x v (HanOpClause op' p' k' m' : r) =
  HanOpClause op' p' k'
    (m' & substituteCom x vTicked)
  : (r & substituteHan x v)
  where
    mx = maxTickCom m'
    vTicked = v & addTickVal (mx + 1)

substituteVal :: VariableIdentifier -> Value -> Value -> Value
substituteVal x v (ValVariable x') | x == x' = v
substituteVal _ _ w@(ValVariable _) = w
substituteVal _ _ w@(ValUnit) = w
substituteVal x v (ValPair v' w') =
  ValPair
    (v' & substituteVal x v)
    (w' & substituteVal x v)
substituteVal x v (ValInjection inj v') =
  ValInjection inj
    (v' & substituteVal x v)
substituteVal x v (ValThunk m') =
  ValThunk
    (m' & substituteCom x v)
substituteVal _ _ w@(ValWildcard) = w

-- very simple alpha renaming
-- keep track of extra number for (un)bound variables (the ticker)
-- when substituting into an environment which binds new variables
-- then: get maximum value for this environment, and increase the ticker
--       by this maximum + 1, ensuring no accidental binding

maxTickVal :: Value -> Int
maxTickVal val = go 0 val
  where
    go mx (ValVariable (_, t)) = max mx t
    go mx (ValUnit) = mx
    go mx (ValPair v w) = max (go mx v) (go mx w)
    go mx (ValInjection _ v) = go mx v
    go mx (ValThunk m) = max mx (maxTickCom m)
    go mx (ValWildcard) = mx

maxTickCom :: Computation -> Int
maxTickCom com = go 0 com
  where
    go mx (ComSplit v m) = max (maxTickVal v) (go mx m)
    go mx (ComCase0 v) = max mx (maxTickVal v)
    go mx (ComCase v m n) = maximum [maxTickVal v, go mx m, go mx n]
    go mx (ComForce v) = max mx (maxTickVal v)
    go mx (ComReturn v) = max mx (maxTickVal v)
    go mx (ComLet (_, t) m n) = maximum [t, go mx m, go mx n]
    go mx (ComLambda (_, t) m) = maximum [t, go mx m]
    go mx (ComLambdaApply m n) = max (go mx m) (maxTickVal n)
    go mx (ComOperationApply _ v) = max mx (maxTickVal v)
    go mx (ComHandle m h) = maximum [go mx m, maxTickHan h]

maxTickHan :: Handler -> Int
maxTickHan han = go 0 han
 where
   go mx [] = mx
   go mx (HanValClause (_, t) m : r) =
     go (maximum [mx, t, maxTickCom m]) r
   go mx (HanOpClause _ (_, t1) (_, t2) m : r) =
     go (maximum [mx, t1, t2, maxTickCom m]) r

addTickVal :: Int -> Value -> Value
addTickVal x (ValVariable (v, t)) = ValVariable (v, t + x)
addTickVal _ v@(ValUnit) = v
addTickVal x (ValPair v w) = ValPair (v & addTickVal x) (w & addTickVal x)
addTickVal x (ValInjection inj v) = ValInjection inj (v & addTickVal x)
addTickVal x (ValThunk m) = ValThunk (m & addTickCom x)
addTickVal _ v@(ValWildcard) = v

addTickCom :: Int -> Computation -> Computation
addTickCom x (ComSplit v m) = ComSplit (v & addTickVal x) (m & addTickCom x)
addTickCom x (ComCase0 v) = ComCase0 (v & addTickVal x)
addTickCom x (ComCase v m n) = ComCase
  (v & addTickVal x)
  (m & addTickCom x)
  (n & addTickCom x)
addTickCom x (ComForce v) = ComForce (v & addTickVal x)
addTickCom x (ComReturn v) = ComReturn (v & addTickVal x)
addTickCom x (ComLet (v, t) m n) = ComLet
  (v, t + x)
  (m & addTickCom x)
  (n & addTickCom x)
addTickCom x (ComLambda (v, t) m) = ComLambda (v, t + x) (m & addTickCom x)
addTickCom x (ComLambdaApply m v) = ComLambdaApply (m & addTickCom x) (v & addTickVal x)
addTickCom x (ComOperationApply op v) = ComOperationApply op (v & addTickVal x)
addTickCom x (ComHandle m h) = ComHandle (m & addTickCom x) (h & addTickHan x)

addTickHan :: Int -> Handler -> Handler
addTickHan _ [] = []
addTickHan x (HanValClause (v, t) m : r) =
  HanValClause (v, t + x)
    (m & addTickCom x)
  : (r & addTickHan x)
addTickHan x (HanOpClause op (p, t1) (k, t2) m : r) =
  HanOpClause op (p, t1 + x) (k, t2 + x)
    (m & addTickCom x)
  : (r & addTickHan x)

testAlphaRename :: IO (Maybe Computation)
testAlphaRename = evalCom [] $
  ComLambdaApply
    (ComLambda ("x", 0) (ComLambda ("z", 0) (ComForce (ValVariable ("x", 0)))))
    (ValVariable ("z", 0))
