{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Parser
import Language.Syntax
import Language.Evaluate

main :: IO ()
main = do
  prog :: String <- readFile "prog.txt"
  let parseResult = parse prog
  print parseResult
