module Parser.IndexSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Index.Parse
import Index.AST
import Parser
import PrettyPrinter
import Control.Monad (liftM, liftM2)


parseIndex = runParser indexExpression True True "test"

spec :: Spec
spec = do
  describe "Index parser" $ do
    context "when parsing infix operators" $ do
      it "parses identical operators left-associatively" $ do
        parseIndex "1 + 2 + 3" `shouldBe` Right (((Number 1) `Plus` (Number 2)) `Plus` (Number 3))
        parseIndex "i * j * k * z" `shouldBe` Right (((IVar "i" `Mult` IVar "j") `Mult` IVar "k" `Mult` IVar "z"))
        parseIndex "10 - top - bot" `shouldBe` Right (((Number 10) `Minus` (IVar "top")) `Minus` (IVar "bot"))
      it "parses '*' before '+'" $ do
        parseIndex "6 + 2 * 5" `shouldBe` Right ((Number 6 `Plus` (Number 2 `Mult` Number 5)))
        parseIndex "6 * 2 + 5" `shouldBe` Right (((Number 6 `Mult` Number 2) `Plus` Number 5))
      it "parses '*' before '-'" $ do
        parseIndex "2 - 3 * 4" `shouldBe` Right ((Number 2 `Minus` (Number 3 `Mult` Number 4)))
        parseIndex "2 * 3 - 4" `shouldBe` Right (((Number 2 `Mult` Number 3) `Minus` Number 4))
      it "parses '-' before '+'" $ do
        parseIndex "1 + 2 - 3" `shouldBe` Right ((Number 1 `Plus` (Number 2 `Minus` Number 3)))
        parseIndex "1 - 2 + 3" `shouldBe` Right (((Number 1 `Minus` Number 2) `Plus` Number 3))
    context "when parsing prefix operators" $ do
      it "parses them after all infix operators" $ do
        parseIndex "max[i<10] i + 12 - 4 * 3" `shouldBe` Right (BoundedMax "i" (Number 10) (IVar "i" `Plus` (Number 12 `Minus` (Number 4 `Mult` Number 3))))
        parseIndex "sum[i<100] i + 12 - 4 * 3" `shouldBe` Right (BoundedSum "i" (Number 100) (IVar "i" `Plus` (Number 12 `Minus` (Number 4 `Mult` Number 3))))
--   describe "Index pretty printer" $ do
--     prop "is the inverse of the parser" $ \index -> (parseIndex . pretty) index `shouldBe` Right index

-- instance Arbitrary Index where
--   arbitrary = sized randomIndex
--     where
--       randomIndex 0 = oneof [
--         Number <$> choose (0,100), 
--         return $ IVar "i"
--         ]
--       randomIndex n = let subIndex = randomIndex (n `div` 2) in
--         oneof [
--           liftM2 Plus subIndex subIndex,
--           liftM2 Minus subIndex subIndex,
--           liftM2 Mult subIndex subIndex,
--           liftM2 Max subIndex subIndex,
--           liftM2 (BoundedMax "i") subIndex subIndex,
--           liftM2 (BoundedSum "i") subIndex subIndex
--         ]


