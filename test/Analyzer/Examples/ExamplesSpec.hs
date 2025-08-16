module Analyzer.Examples.ExamplesSpec (spec) where

import Test.Hspec
import TestUtil
import PQ.Prelude
import Metric
import System.FilePath

programDir :: FilePath
programDir = "test" </> "Analyzer" </> "Examples"

positiveDir :: FilePath
positiveDir = programDir </> "pos"

negativeDir :: FilePath
negativeDir = programDir </> "neg"

spec :: Spec
spec = do
  around (withSolver Nothing) $ do
    describe "analyzer" $ do
      it "accepts the dumbNot program" $ \qfh -> do
        withTest (positiveDir </> "dumbNot.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "rejects the wrong dumbNot program" $ \qfh -> do
        withTest (negativeDir </> "dumbNot.pq") $ \test -> do
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldReject` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldReject` test
      it "accepts the teleportation program" $ \qfh -> do
        withTest (positiveDir </> "teleportation.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "rejects the wrong teleportation program" $ \qfh -> do
        withTest (negativeDir </> "teleportation.pq") $ \test -> do
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldReject` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldReject` test
      it "accepts the QFT program" $ \qfh -> do
        withTest (positiveDir </> "qft.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "rejects the wrong QFT program" $ \qfh -> do
        withTest (negativeDir </> "qft.pq") $ \test -> do
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldReject` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldReject` test
      it "accepts the Grover program" $ \qfh -> do
        withTest (positiveDir </> "grover.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just gateCountMetric) Nothing [prelude] qfh `shouldAccept` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldAccept` test
          analysisTest (Just gateCountMetric) (Just depthMetric) [prelude] qfh `shouldAccept` test
      it "rejects the wrong Grover program" $ \qfh -> do
        withTest (negativeDir </> "grover.pq") $ \test -> do
          analysisTest (Just gateCountMetric) Nothing [prelude] qfh `shouldReject` test
          analysisTest Nothing (Just depthMetric) [prelude] qfh `shouldReject` test
      it "accepts the adder program" $ \qfh -> do
        withTest (positiveDir </> "adder.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldAccept` test
      it "rejects the wrong adder program" $ \qfh -> do
        withTest (negativeDir </> "adder.pq") $ \test -> do
          typeCheckingTest [prelude] qfh `shouldAccept` test
          analysisTest (Just widthMetric) Nothing [prelude] qfh `shouldReject` test