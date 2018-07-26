module Language.Parser where

import Language.Syntax

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

names =
  [ "let"
  , "in"
  , "in_0"
  , "in_1"
  , "split"
  , "case"
  , "case0"
  , "handle"
  , "with"
  , "return"
  ]

opNames =
  [ "let"
  , "in"
  , "in_0"
  , "in_1"
  , "split"
  , "case"
  , "case0"
  , "handle"
  , "with"
  , "return"
  , "<-"
  , ":"
  , "|"
  , "->"
  ]

languageDef :: LanguageDef st
languageDef = emptyDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = names
  , Token.reservedOpNames = opNames
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
symbol = Token.symbol lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
whiteSpace = Token.whiteSpace lexer

varIdent :: Parser VariableIdentifier
varIdent = (\x -> (x, 0)) <$> identifier

data Term
 = Comp Computation
 | Val Value
 | Op OperationIdentifier
 deriving (Eq, Show, Ord)

parseTerm :: Parser Term
parseTerm =
  buildExpressionParser ops atom
  where
  -- operations from higher to lower priority
  ops =
    [ [ postfix "!" (\(Val v) -> Comp $ ComForce v)
      ]
    , [ application
      ]
    , [ prefix "in_0" (\(Val v) -> Val $ ValInjection Inj0 v)
      , prefix "in_1" (\(Val v) -> Val $ ValInjection Inj1 v)
      ]
    ]
  atom = msum
    [ (Val . ValVariable) <$> varIdent
    , Op <$> (symbol ":" *> identifier)
    , pure (Val ValUnit) <* reserved "()"
    , (Val . ValThunk) <$> (braces ((\(Comp m) -> m) <$> parseTerm))
    , try valPair
    , comLet
    , comLambda
    , comHandle
    , comSplit
    , comCase
    , comCase0
    , comReturn
    , parens parseTerm
    ]

application = Infix space AssocLeft
  where
  space =
    whiteSpace *>
    notFollowedBy (choice . map reservedOp $ opNames) *>
    return (\a b -> Comp $ app a b)
  app (Comp m) (Val v) = ComLambdaApply m v
  app (Op op)  (Val v) = ComOperationApply op v

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

valPair = parens (do
  (Val v) <- parseTerm
  reserved ","
  (Val w) <- parseTerm
  return $ Val $ ValPair v w
  )

comLet = do
  reserved "let"
  x <- varIdent
  reservedOp "<-"
  (Comp m) <- parseTerm
  reserved "in"
  (Comp n) <- parseTerm
  return $ Comp $ ComLet x m n

comLambda = do
  symbol "\\"
  whiteSpace
  x <- varIdent
  symbol "."
  whiteSpace
  (Comp m) <- parseTerm
  return $ Comp $ ComLambda x m

comHandle = do
  reserved "handle"
  (Comp m) <- parseTerm
  reserved "with"
  clauses <- handlerClauses
  return $ Comp $ ComHandle m clauses

handlerClauses :: Parser [HandlerClause]
handlerClauses = many (hanValClause <|> hanOpClause)

hanValClause = do
  try $ do
          reserved "|"
          reserved "return"
  x <- varIdent
  reserved "->"
  (Comp m) <- parseTerm
  return $ HanValClause x m

hanOpClause = do
  reserved "|"
  symbol ":"
  op <- identifier
  p <- varIdent
  k <- varIdent
  reserved "->"
  (Comp m) <- parseTerm
  return $ HanOpClause op p k m

comSplit = reserved "split" *> parens (do
  (Val v) <- parseTerm
  reserved ","
  (Comp m) <- parseTerm
  return $ Comp $ ComSplit v m
  )

comCase = reserved "case" *> parens (do
  (Val v) <- parseTerm
  reserved ","
  (Comp m) <- parseTerm
  reserved ","
  (Comp n) <- parseTerm
  return $ Comp $ ComCase v m n
  )

comCase0 = reserved "case0" *> parens (do
  (Val v) <- parseTerm
  return $ Comp $ ComCase0 v
  )

comReturn =
  (\(Val v) -> Comp $ ComReturn v) <$> (reserved "return" *> parseTerm)

topLevel :: Parser TopLevelDeclaration
topLevel = do
  x <- identifier
  symbol "="
  whiteSpace
  (Val v) <- parseTerm
  symbol ";"
  return $ TLDeclaration x v

eol :: Parser ()
eol = void (char '\n') <|> eof

parseProgram :: Parser Program
parseProgram = many topLevel

--

parseString :: String -> Either ParseError Program
parseString s = parse (parseProgram <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile f = parseFromFile (parseProgram <* eof) f

try2 :: String -> IO ()
try2 x = do
  parseTest (whiteSpace *> parseProgram <* whiteSpace <* eof) x
