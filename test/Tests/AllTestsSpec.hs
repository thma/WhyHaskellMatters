module Tests.AllTestsSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Functions
import           LazyEvaluation
import           AlgebraicDataTypes

spec :: Spec
spec =
  describe "Why Haskell matters" $ do
    it "it has functions" $
      property $ \x -> square x `shouldBe` x*x
    it "can handle user defined controll structures" $
      cond [(False, viciousCircle), 
            (True, 3), 
            (False, viciousCircle)] `shouldBe` 3
    it "can build trees" $
      show (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) `shouldBe` "Node (Leaf 1) (Node (Leaf 2) (Leaf 3))"