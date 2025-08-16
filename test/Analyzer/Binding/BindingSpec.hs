module Analyzer.Binding.BindingSpec (spec) where

import Test.Hspec
import TestUtil
import Control.Monad
import System.FilePath

programDir :: FilePath
programDir = "test" </> "Analyzer" </> "Binding"

positiveDir :: FilePath
positiveDir = programDir </> "pos"

negativeDir :: FilePath
negativeDir = programDir </> "neg"

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "binding checker" $ do
      it "accepts correct programs" $ \qfh -> do
        withTests positiveDir $ \tests -> do
          when (null tests) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldAccept` test
      it "rejects incorrect programs" $ \qfh -> do
        withTests negativeDir $ \tests -> do
          when (null tests) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldReject` test