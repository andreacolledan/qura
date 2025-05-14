module Analyzer.Subtype.SubtypeSpec (spec) where

import Test.Hspec
import TestUtil
import Control.Monad

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "subtype checker" $ do
      it "accepts correct programs" $ \qfh -> do
        withTests "test/Analyzer/Subtype/pos/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            widthCheckingTest [] qfh `shouldAccept` test
      it "rejects incorrect programs" $ \qfh -> do
        withTests "test/Analyzer/Subtype/neg/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            widthCheckingTest [] qfh `shouldReject` test


