module Common.SplitSpec (spec) where

import Test.Hspec
import Common.Split

spec :: Spec
spec = do
  describe "When " $ do
    it "Should split Strings list" $ do
      splitPairOn [""] ["a", "b", "", "c"] `shouldBe` (["a", "b"], ["c"])
