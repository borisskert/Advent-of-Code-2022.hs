module Day03.ItemSpec (spec) where

import Day03.Item
import Test.Hspec

spec :: Spec
spec = do
  describe "Should priorize Items" $ do
    it "16 (p)" $ do
      (priority . from $ 'p') `shouldBe` 16
    it "42 (P)" $ do
      (priority . from $ 'P') `shouldBe` 42
