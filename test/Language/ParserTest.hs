module Language.ParserTest where

import Language.Parser

import Test.Tasty
import Test.Tasty.Golden

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (pack)
import qualified System.FilePath as Path

test_goldenTests :: IO TestTree
test_goldenTests = do
  flFiles <- findByExtension [".fl"] "test/golden_files/parser/"
  return $ testGroup "parser golden tests"
    [ goldenVsString
        ("ParserTest -- " ++ Path.takeBaseName flFile)
        resultFile
        (testFunc flFile)
    | flFile <- flFiles
    , let resultFile = Path.replaceExtension flFile ".txt"
    ]

testFunc :: FilePath -> IO LBS.ByteString
testFunc file = do
  parsedResult <- parseFile file
  case parsedResult of
    Right parsedProgram -> return (pack (show parsedProgram))
    Left parseErr -> return (pack (show parseErr))
