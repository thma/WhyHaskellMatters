module Tests.AllTestsSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Functions

spec :: Spec
spec =
  describe "Why Haskell matters" $ do
    it "it has functions" $
     property $ \x -> square x `shouldBe` x*x
