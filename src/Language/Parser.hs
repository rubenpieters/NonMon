{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExplicitForAll#-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Parser where

import Language.Syntax

import Data.Char
import Control.Applicative
import Text.Earley

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

keywords :: HashSet String
keywords = HS.fromList ["in_0", "in_1", "split", "case0", "case", "return", "let", "in"]

notIn :: HashSet String -> String -> Bool
notIn hs s = not (HS.member s hs)

grammar :: Grammar r (Prod r String Char Value)
grammar = mdo
  whitespace <- rule $ many $ satisfy isSpace

  let --tok :: Prod r String Char a -> Prod r String Char a
      tok p   = whitespace *> p

      sym x   = tok $ token x <?> [x]
      ident   = tok $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum) <?> "identifier"
      num     = tok $ some (satisfy isDigit) <?> "number"
      syms w  = tok $ list w


  val <- rule
     $  ValVariable <$> ident
    <|> ValPair <$> (sym '(' *> val)
                <*> (sym ',' *> val <* sym ')')
    <|> ValInjection <$> (list "in_0" *> pure Inj0)
                     <*> val
    <|> ValInjection <$> (list "in_1" *> pure Inj1)
                     <*> val
    <|> pure ValUnit <* (sym '(' *> sym ')')
    <|> ValThunk <$> (sym '{' *> comp <* sym '}')

  comp <- rule
     $  ComForce <$> (val <* sym '!')
    <|> ComSplit <$> (list "split" *> sym '(' *> val)
                 <*> (sym ',' *> ident)
                 <*> (sym '.' *> ident)
                 <*> (sym '.' *> comp <* sym ')')
    <|> ComCase0 <$> (list "case0" *> sym '(' *> val <* sym ')')
    <|> ComCase <$> (list "case" *> sym '(' *> val)
                <*> (sym ',' *> ident)
                <*> (sym '.' *> comp)
                <*> (sym ',' *> ident)
                <*> (sym '.' *> comp <* sym ')')
    <|> ComReturn <$> (list "return" *> val)
    <|> ComLet <$> (list "let" *> ident)
               <*> (syms "<-" *> comp)
               <*> (syms "in" *> comp)
    <|> ComLambda <$> (sym '\\' *> ident)
                  <*> (sym '.' *> comp)
    <|> ComLambdaApply <$> comp
                       <*> (sym ' ' *> val)
    <|> ComOperationApply <$> (sym ':' *> ident)
                          <*> (sym ' ' *> val)
    <|> ComHandle <$> (list "handle" *> comp)
                  <*> (syms "with" *> many handlerClause)

  handlerClause <- rule
     $  HanValClause <$> (sym '|' *> whitespace *> list "return" *> ident)
                     <*> (syms "->" *> comp)
    <|> HanOpClause <$> (sym '|' *> sym ':' *> ident)
                    <*> (sym ' ' *> ident)
                    <*> (sym ' ' *> ident)
                    <*> (syms "->" *> comp)

  return $ val <* whitespace


--    parse = fullParses (parser grammar)
--    parseResult = head . fst . parse

try :: String -> IO ()
try x = do
  print (fullParses (parser grammar) x)
