module Analyzer.Binding.Spec where

import Test.Hspec

spec :: Spec
spec = describe "binding checker" $ do
  it "works" $ do
    1 `shouldBe` 1