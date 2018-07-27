module Language.LanguageTest where

import Language.Parser
import Language.Evaluate
import Language.Printer

import Test.Tasty
import Test.Tasty.Golden

import Data.Traversable (for)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (pack)
import qualified System.FilePath as Path

test_goldenTests :: IO [TestTree]
test_goldenTests = do
  flFiles <- findByExtension [".fl"] "test/golden_files/"
  return
    [ testGroup "Parser Tests" (goldenParser <$> flFiles)
    , testGroup "Evaluate Tests" (goldenEvaluate <$> flFiles)
    ]

goldenParser :: FilePath -> TestTree
goldenParser file = goldenVsString
  ("ParserTest -- " ++ Path.takeBaseName file)
  (Path.replaceExtension file ".fl.parsed")
  (testParser file)

testParser :: FilePath -> IO LBS.ByteString
testParser file = do
  parsedResult <- parseFile file
  case parsedResult of
    Right parsedProgram -> return (pack (show parsedProgram))
    Left parseErr -> return (pack (show parseErr))

goldenEvaluate :: FilePath -> TestTree
goldenEvaluate file = goldenVsString
  ("EvaluateTest -- " ++ Path.takeBaseName file)
  (Path.replaceExtension file ".fl.eval")
  (testEvaluate file)

testEvaluate :: FilePath -> IO LBS.ByteString
testEvaluate file = do
  parsedResult <- parseFile file
  case parsedResult of
    Right parsedProgram -> do
      let mainDef = findMain parsedProgram
      evaledMain <- repeatEval parsedProgram mainDef
      return (pack (prettyCom evaledMain))
    Left parseErr -> return (pack (show parseErr))

