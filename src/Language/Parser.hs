module Parser where

import Text.Parsec.Language
import Text.Parsec.Token as Token
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

reservedNames' :: [String]
reservedNames' =
  [ "inj0"
  , "inj1"
  , "split"
  , "case0"
  , "case"
  , "let"
  , "in"
  ]

reservedOpNames' :: [String]
reservedOpNames' =
  [ "!"
  ]

languageDef :: LanguageDef st
languageDef = emptyDef
  { commentStart = "{-"
  , commentEnd = "-}"
  , commentLine = "--"
  , reservedNames = reservedNames'
  , reservedOpNames = reservedOpNames'
  }

lexer = makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer


