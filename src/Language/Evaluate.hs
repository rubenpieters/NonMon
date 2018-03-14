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

-- TODO: alpha rename before substitution, to prevent accidental binding
substituteCom :: String -> Value -> Computation -> Computation
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
  error "alpha rename"
substituteCom x v (ComLambda x' m') =
  error "alpha rename"
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

substituteHan :: String -> Value -> Handler -> Handler
substituteHan _ _ [] = []
substituteHan x v ((HanValClause x' m'):r) =
  HanValClause x'
    (error "alpha rename")
  : (r & substituteHan x v)
substituteHan x v ((HanOpClause op' p' k' m'):r) =
  HanOpClause op' p' k'
    (error "alpha rename")
  : (r & substituteHan x v)

substituteVal :: String -> Value -> Value -> Value
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
