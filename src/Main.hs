{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase#-}

module Main where

import Text.Parsec

import Language.Parser
import Language.Syntax
import Language.Evaluate
import Language.Printer

main :: IO ()
main = run "prog.txt"

run :: String -> IO ()
run file = do
  (parseResult :: Program) <-
    parseFile file >>= \case
      Left err -> error (show err)
      Right prog -> return prog
  putStrLn "| parsed:"
  print parseResult
  putStrLn "| eval:"
  let (mainDef :: Computation) = findMain parseResult
  (evaledMain :: Computation) <- repeatEval parseResult mainDef
  putStrLn "| evaled:"
  print evaledMain
  putStrLn "| evaled pretty:"
  print (prettyCom evaledMain)

findMain :: Program -> Computation
findMain [] = error "could not find main"
findMain (TLDeclaration "main" (ValThunk com) : _) = com
findMain (TLDeclaration "main" _ : _) = error "main should be a thunk"
findMain (_ : r) = findMain r
