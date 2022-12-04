module Day04.AssignmentSpec (spec) where

import Day04.Assignment
import Test.Hspec

spec :: Spec
spec = do
  describe "Specify how Assignments contains each other" $ do
    it "2-8 should contain 4-6" $ do
      (fromTo 2 8 `contains` fromTo 4 6) `shouldBe` True

    it "2-8 should contain 2-8" $ do
      (fromTo 2 8 `contains` fromTo 2 8) `shouldBe` True

    it "2-8 should contain 2-4" $ do
      (fromTo 2 8 `contains` fromTo 2 4) `shouldBe` True

    it "2-8 should contain 6-8" $ do
      (fromTo 2 8 `contains` fromTo 6 8) `shouldBe` True

    it "2-3 should NOT contain 3-8" $ do
      (fromTo 2 3 `contains` fromTo 3 8) `shouldBe` False

    it "2-3 should NOT contain 4-8" $ do
      (fromTo 2 3 `contains` fromTo 4 8) `shouldBe` False

    it "3-8 should NOT contain 1-2" $ do
      (fromTo 2 8 `contains` fromTo 1 2) `shouldBe` False
