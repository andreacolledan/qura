module Analyzer.Binding.BindingSpec where

import Test.Hspec
import TestUtil
import Control.Monad

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "binding checker" $ do
      it "accepts correct programs" $ \qfh -> do
        withTests "test/Analyzer/Binding/pos/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldAccept` test
      it "rejects incorrect programs" $ \qfh -> do
        withTests "test/Analyzer/Binding/neg/" $ \tests -> do
          when (length tests == 0) pending
          forM_ tests $ \test -> do
            typeCheckingTest [] qfh `shouldReject` test