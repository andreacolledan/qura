module Analyzer.ExamplesSpec (spec) where

import Test.Hspec
import TestUtil
import Lang.Library.Prelude
import Index.Semantics.Global.Width
import Index.Semantics.Global.GateCount
import Index.Semantics.Local.Depth

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "analyzer" $ do
      it "accepts the dumbNot program" $ \qfh -> do
        withTest "examples/dumbNot.pq" $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "accepts the teleportation program" $ \qfh -> do
        withTest "examples/teleportation.pq" $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "accepts the QFT program" $ \qfh -> do
        withTest "examples/qft.pq" $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "accepts the Grover program" $ \qfh -> do
        withTest "examples/grover.pq" $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just gateCountMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just gateCountMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "accepts the adder program" $ \qfh -> do
        withTest "examples/adder.pq" $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test