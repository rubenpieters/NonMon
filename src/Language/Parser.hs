{-# LANGUAGE RecursiveDo #-}

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

-- TODO: increase tick each time variable is shadowed
-- for example in "test={\x.(\x.x!)}"

grammar :: Grammar r (Prod r String Char Program)
grammar = mdo
  whitespace <- rule $ many $ satisfy isSpace

  let --tok :: Prod r String Char a -> Prod r String Char a
      tok p   = whitespace *> p

      sym x   = tok $ token x <?> [x]
      ident   = tok $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum) <?> "identifier"
      ident0  = (\x -> (x, 0)) <$> ident
      num     = tok $ some (satisfy isDigit) <?> "number"
      syms w  = tok $ list w


  val0 <- rule
     $  ValVariable <$> ident0
    <|> ValPair <$> (sym '(' *> val)
                <*> (sym ',' *> val <* sym ')')
    <|> ValInjection <$> (list "in_0" *> pure Inj0)
                     <*> val
    <|> ValInjection <$> (list "in_1" *> pure Inj1)
                     <*> val
    <|> pure ValUnit <* (syms "()")
    <|> ValThunk <$> (sym '{' *> comp <* sym '}')
    <|> pure ValWildcard <* (sym '?')
    <?> "value"

  val <- rule
     $  whitespace *> val0
    <|> sym '(' *> val <* sym ')'

  comp0 <- rule
     $  ComForce <$> (val <* sym '!')
    <|> ComSplit <$> (list "split" *> sym '(' *> val)
                 <*> (sym ',' *> comp <* sym ')')
    <|> ComCase0 <$> (list "case0" *> sym '(' *> val <* sym ')')
    <|> ComCase <$> (list "case" *> sym '(' *> val)
                <*> (sym ',' *> comp)
                <*> (sym ',' *> comp <* sym ')')
    <|> ComReturn <$> (list "return" *> val)
    <|> ComLet <$> (list "let" *> ident0)
               <*> (syms "<-" *> comp)
               <*> (syms "in" *> comp)
    <|> ComLambda <$> (sym '\\' *> ident0)
                  <*> (sym '.' *> comp)
    <|> ComLambdaApply <$> comp
                       <*> (sym ' ' *> val)
    <|> ComOperationApply <$> (sym ':' *> ident)
                          <*> (sym ' ' *> val)
    <|> ComHandle <$> (list "handle" *> comp)
                  <*> (syms "with" *> many handlerClause)
    <?> "computation"

  comp <- rule
     $  whitespace *> comp0
    <|> sym '(' *> comp <* sym ')'

  handlerClause <- rule
     $  HanValClause <$> (sym '|' *> syms "return" *> ident0)
                     <*> (syms "->" *> comp)
    <|> HanOpClause <$> (sym '|' *> sym ':' *> ident)
                    <*> (sym ' ' *> ident0)
                    <*> (sym ' ' *> ident0)
                    <*> (syms "->" *> comp)
    <?> "handler clause"

  toplevel <- rule
     $  TLDeclaration <$> (ident)
                      <*> (sym '=' *> val)

  return $ (many toplevel) <* whitespace


parse :: String -> Program
parse x = head $ fst $ fullParses (parser grammar) x

--    parseResult = head . fst . parse

try :: String -> IO ()
try x = do
  print (fullParses (parser grammar) x)
