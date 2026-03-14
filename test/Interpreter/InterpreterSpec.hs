module Interpreter.InterpreterSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
import Test.Hspec

import Analyzer (runAnalysis)
import Interpreter (InterpreterResult (..), QasmProgram (..), runInterpreter)
import Parser (errorBundlePretty, parseModule, runParser)
import PQ.Prelude (prelude)
import TestUtil (withSolver)

import Interface (CLArguments (..))

programDir :: FilePath
programDir = "test" </> "Interpreter"

positiveDir :: FilePath
positiveDir = programDir </> "pos"

mkArgs :: FilePath -> CLArguments
mkArgs fp =
  CommandLineArguments
    { filepath = fp,
      verbose = False,
      norun = False,
      debug = Nothing,
      noprelude = False,
      grs = Nothing,
      lrs = Nothing,
      qubitRecycling = True,
      outputFilepath = Nothing
    }

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "interpreter" $ do
      files <- runIO $ do
        names <- listDirectory positiveDir
        pure $
          sort
            [ positiveDir </> name
              | name <- names,
                takeExtension name == ".pq"
            ]

      forM_ files $ \fp ->
        it ("runs " ++ takeFileName fp) $ \qfh -> do
          source <- readFile fp
          case runParser parseModule False False fp source of
            Left err ->
              expectationFailure $
                "Parse failed for "
                  ++ fp
                  ++ ":\n"
                  ++ errorBundlePretty err
            Right m -> do
              analyzed <- runAnalysis m [prelude] qfh Nothing Nothing
              case analyzed of
                Left typeErr ->
                  expectationFailure $
                    "Analysis failed for "
                      ++ fp
                      ++ ":\n"
                      ++ show typeErr
                Right analyzedModule ->
                  case runInterpreter analyzedModule [prelude] (mkArgs fp) of
                    Left runtimeErr ->
                      expectationFailure $
                        "Interpreter failed for "
                          ++ fp
                          ++ ":\n"
                          ++ show runtimeErr
                    Right (InterpResult _ _ (QasmProg _ _ instrs)) ->
                      instrs `shouldSatisfy` (not . null)
