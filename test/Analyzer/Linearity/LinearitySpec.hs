module Analyzer.Linearity.LinearitySpec (spec) where

import Test.Hspec
import TestUtil
import Control.Monad

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "linearity checker" $ do
      it "accepts correct programs" $ \qfh -> do
        withTests "test/Analyzer/Linearity/pos/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldAccept` test
      it "rejects incorrect programs" $ \qfh -> do
        withTests "test/Analyzer/Linearity/neg/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldReject` test
