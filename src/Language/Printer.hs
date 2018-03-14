module Language.Printer where

import Language.Syntax

prettyVal :: Value -> String
prettyVal (ValVariable (x, _)) = x
prettyVal (ValUnit) = "()"
prettyVal (ValPair v w) = "(" ++ prettyVal v ++ ", " ++ prettyVal w ++ ")"
prettyVal (ValInjection Inj0 v) = "inj_0(" ++ prettyVal v ++")"
prettyVal (ValInjection Inj1 v) = "inj_1(" ++ prettyVal v ++")"
prettyVal (ValThunk m) = "{" ++ prettyCom m ++ "}"

prettyCom :: Computation -> String
prettyCom (ComSplit v m) = "split(" ++ prettyVal v ++ ", " ++ prettyCom m ++ ")"
prettyCom (ComCase0 v) = "case0(" ++ prettyVal v ++ ")"
prettyCom (ComCase v m n) = "case(" ++ prettyVal v ++ ", " ++ prettyCom m ++ "," ++ prettyCom n ++ ")"
prettyCom (ComForce v) = prettyVal v ++ "!"
prettyCom (ComReturn v) = "return " ++ prettyVal v
prettyCom (ComLet (x, _) m n) = "let " ++ x ++ " <- " ++ prettyCom m ++ " in " ++ prettyCom n
prettyCom (ComLambda (x, _) m) = "\\" ++ x ++ "." ++ prettyCom m
prettyCom (ComLambdaApply m v) = "(" ++ prettyCom m ++ ") " ++ prettyVal v
prettyCom (ComOperationApply op v) = ":" ++ op ++ " " ++ prettyVal v
prettyCom (ComHandle m h) = "handle (" ++ prettyCom m ++ ") with" ++ prettyHan h

prettyHan :: Handler -> String
prettyHan [] = ""
prettyHan (HanValClause (x, _) m : r) = (" | return " ++ x ++ " -> " ++ prettyCom m) ++ prettyHan r
prettyHan (HanOpClause op (p, _) (k, _) m : r) = (" | :" ++ op ++ " " ++ p ++ " " ++ k ++ " -> " ++ prettyCom m) ++ prettyHan r
