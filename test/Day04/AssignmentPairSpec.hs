module Day04.AssignmentPairSpec (spec) where

import Day04.Assignment (fromTo)
import Day04.AssignmentPair
import Test.Hspec

spec :: Spec
spec = do
  describe "Should readOne from input" $ do
    it "Should readOne from '2-4,6-8'" $ do
      readOne "2-4,6-8" `shouldBe` pairOf (fromTo 2 4) (fromTo 6 8)

  describe "Some of the pairs have noticed that one of their assignments fully contains the other" $ do
    it "For example, 2-8 fully contains 3-7" $ do
      isFullyContained (pairOf (fromTo 2 8) (fromTo 3 7)) `shouldBe` True

    it "and 6-6 is fully contained by 4-6" $ do
      isFullyContained (pairOf (fromTo 6 6) (fromTo 4 6)) `shouldBe` True

    it "and 5-7 is not fully contained by 7-9 or vise-versa" $ do
      isFullyContained (pairOf (fromTo 5 7) (fromTo 7 9)) `shouldBe` False
