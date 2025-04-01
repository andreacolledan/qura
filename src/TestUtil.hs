module TestUtil (
  withTests,
  basicAnalysisTest,
  shouldAccept,
  shouldReject,
  --re-exports
  withSolver
) where

import Test.Hspec
import System.Directory
import Solving.CVC5
import Lang.Analysis
import Lang.Module.Parse
import Parser

data TestOutcome = Unparsed ParserError | Fail TypeError | Pass deriving (Eq, Show)

withTests :: String -> ([(String, String)] -> IO ()) -> IO ()
withTests which action = do
  filenames <- listDirectory which
  let filepaths = map (which ++ ) filenames
  filecontents <- mapM readFile filepaths
  action (zip filepaths filecontents)

basicAnalysisTest :: SolverHandle -> (String, String) -> IO (String, TestOutcome)
basicAnalysisTest qfh (filepath, source) = do
  case runParser parseModule False False filepath source of
    Left err -> return (filepath, Unparsed err)
    Right mod -> do
      outcome <- runAnalysis mod [] qfh Nothing Nothing
      case outcome of
        Left err -> return (filepath, Fail err)
        Right _ -> return (filepath, Pass)
  
shouldAccept :: ((String, String) -> IO (String, TestOutcome)) -> (String, String) -> Expectation
shouldAccept action test = action test >>= (`shouldSatisfy` passed)
  where
    passed (_, Pass) = True
    passed _ = False

shouldReject :: ((String, String) -> IO (String, TestOutcome)) -> (String, String) -> Expectation
shouldReject action test = action test >>= (`shouldSatisfy` failed)
  where
    failed (_, Fail _) = True
    failed _ = False