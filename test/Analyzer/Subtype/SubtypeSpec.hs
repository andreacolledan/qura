module Analyzer.Subtype.SubtypeSpec (spec) where

import Test.Hspec
import TestUtil
import Control.Monad
import System.FilePath

programDir :: FilePath
programDir = "test" </> "Analyzer" </> "Subtype"

positiveDir :: FilePath
positiveDir = programDir </> "pos"

negativeDir :: FilePath
negativeDir = programDir </> "neg"

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "subtype checker" $ do
      it "accepts correct programs" $ \qfh -> do
        withTests positiveDir $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            widthCheckingTest [] qfh `shouldAccept` test
      it "rejects incorrect programs" $ \qfh -> do
        withTests negativeDir $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            widthCheckingTest [] qfh `shouldReject` test


