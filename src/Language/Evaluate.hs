module Language.Evaluate where

import Data.Function ((&))

import Language.Syntax

evalCom :: Computation -> Computation
-- split((v,w), \x.\y.m)
-- m[x=v,y=w]
evalCom (ComSplit (ValPair v w) (ComLambda x (ComLambda y m))) =
  m & substituteCom x v
    & substituteCom y w
--evalCom (ComCase0 ()) =
-- case(in_0 v, \x.m, \y.n)
-- m[x=v]
evalCom (ComCase (ValInjection Inj0 v) (ComLambda x m) _) =
  m & substituteCom x v
-- case(in_1 v, \x.m, \y.n)
-- n[y=v]
evalCom (ComCase (ValInjection Inj1 v) (ComLambda y n) _) =
  n & substituteCom y v
-- {m}!
-- m
evalCom (ComForce (ValThunk m)) = m
-- handle (return v) with | return x -> m
-- m[x=v]

-- let x <- (return V) in n
-- n[x=v]
evalCom (ComLet x (ComReturn v) n) =
  n & substituteCom x v
-- (\x.m) v
-- m[x=v]
evalCom (ComLambdaApply (ComLambda x m) v) =
  m & substituteCom x v

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
addTickVal x v@(ValUnit) = v
addTickVal x (ValPair v w) = ValPair (v & addTickVal x) (w & addTickVal x)
addTickVal x (ValInjection inj v) = ValInjection inj (v & addTickVal x)
addTickVal x (ValThunk m) = ValThunk (m & addTickCom x)

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

testAlphaRename :: Computation
testAlphaRename = evalCom $
  ComLambdaApply
    (ComLambda ("x", 0) (ComLambda ("z", 0) (ComForce (ValVariable ("x", 0)))))
    (ValVariable ("z", 0))
