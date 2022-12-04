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

  describe "Specify how Assing overlap" $ do
    it "5-7,7-9 overlaps in a single section, 7" $ do
      (fromTo 5 7 `overlaps` fromTo 7 9) `shouldBe` True

    it "7-9,5-7 overlaps in a single section, 7" $ do
      (fromTo 7 9 `overlaps` fromTo 5 7) `shouldBe` True

    it "2-8,3-7 overlaps all of the sections 3 through 7" $ do
      (fromTo 2 8 `overlaps` fromTo 3 7) `shouldBe` True

    it "3-7,2-8 overlaps all of the sections 3 through 7" $ do
      (fromTo 3 7 `overlaps` fromTo 2 8) `shouldBe` True

    it "6-6,4-6 overlaps in a single section, 6" $ do
      (fromTo 6 6 `overlaps` fromTo 4 6) `shouldBe` True

    it "4-6,6-6 overlaps in a single section, 6" $ do
      (fromTo 4 6 `overlaps` fromTo 6 6) `shouldBe` True

    it "2-6,4-8 overlaps in sections 4, 5, and 6" $ do
      (fromTo 2 6 `overlaps` fromTo 4 8) `shouldBe` True

    it "4-8,2-6 overlaps in sections 4, 5, and 6" $ do
      (fromTo 4 8 `overlaps` fromTo 2 6) `shouldBe` True

    it "2-4,6-8 don't overlap" $ do
      (fromTo 2 4 `overlaps` fromTo 6 8) `shouldBe` False

    it "6-8,2-4 don't overlap" $ do
      (fromTo 6 8 `overlaps` fromTo 2 4) `shouldBe` False

    it "2-3,4-5 don't overlap" $ do
      (fromTo 2 3 `overlaps` fromTo 4 5) `shouldBe` False

    it "4-5,2-3 don't overlap" $ do
      (fromTo 4 5 `overlaps` fromTo 2 3) `shouldBe` False
