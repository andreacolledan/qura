module TestUtil (
  withTest,
  withTests,
  analysisTest,
  typeCheckingTest,
  shouldAccept,
  shouldReject,
  --re-exports
  withSolver
) where

import Data.Maybe
import Index.Semantics.Global.Resource
import Index.Semantics.Local.Resource
import Lang.Analysis
import Lang.Module.AST
import Lang.Module.Parse
import Parser
import Solving.CVC5
import System.Directory
import System.FilePath
import Test.Hspec

data TestOutcome = Unparsed ParserError | Fail TypeError | Pass deriving (Eq, Show)

withTest :: FilePath -> ((FilePath, String) -> Expectation) -> Expectation
withTest which action = readFile which >>= action . (,) which

withTests :: FilePath -> ([(FilePath, String)] -> Expectation) -> Expectation
withTests dir action = do
  filenames <- listDirectory dir
  let filepaths = map (dir </>) filenames
  filecontents <- mapM readFile filepaths
  action (zip filepaths filecontents)

analysisTest :: Maybe GlobalMetricModule -> Maybe LocalMetricModule -> [Module] -> SolverHandle -> (FilePath, String) -> IO (FilePath, TestOutcome)
analysisTest mgmm mlmm libs qfh (filepath, source) = do
  case runParser parseModule (isJust mgmm) (isJust mlmm) filepath source of
    Left err -> return (filepath, Unparsed err)
    Right mod -> do
      outcome <- runAnalysis mod libs qfh mgmm mlmm
      case outcome of
        Left err -> return (filepath, Fail err)
        Right _ -> return (filepath, Pass)

typeCheckingTest :: [Module] -> SolverHandle -> (FilePath, String) -> IO (String, TestOutcome)
typeCheckingTest = analysisTest Nothing Nothing

shouldAccept :: ((FilePath, String) -> IO (FilePath, TestOutcome)) -> (FilePath, String) -> Expectation
shouldAccept action test = action test >>= (`shouldSatisfy` passed)
  where
    passed (_, Pass) = True
    passed _ = False

shouldReject :: ((FilePath, String) -> IO (FilePath, TestOutcome)) -> (FilePath, String) -> Expectation
shouldReject action test = action test >>= (`shouldSatisfy` failed)
  where
    failed (_, Fail _) = True
    failed _ = False