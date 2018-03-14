{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Parser
import Language.Syntax
import Language.Evaluate
import Language.Printer

main :: IO ()
main = do
  prog :: String <- readFile "prog.txt"
  let (parseResult :: Program) = parse prog
  putStrLn "parsed"
  print parseResult
  let (mainDef :: Computation) = findMain parseResult
  let (evaledMain :: Computation) = repeatEval parseResult mainDef
  putStrLn "evaled"
  print evaledMain
  putStrLn "evaled pretty"
  print (prettyCom evaledMain)

findMain :: Program -> Computation
findMain [] = error "could not find main"
findMain (TLDeclaration "main" (ValThunk com) : _) = com
findMain (TLDeclaration "main" _ : _) = error "main should be a thunk"
findMain (_ : r) = findMain r

