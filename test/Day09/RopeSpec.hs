module Day09.RopeSpec (spec) where

import Common.OctaGrid (Position)
import Day09.Rope (fromList, toList)
import qualified Day09.Rope as Rope (moveTail)
import Test.Hspec

moveTail :: Position -> [Position] -> [Position]
moveTail headPos = toList . Rope.moveTail headPos . fromList

spec :: Spec
spec = do
  describe "When moveTail" $ do
    it "Should moveTail step 1" $ do
      moveTail (4, 1) [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0), (0, 0), (0, 0)] `shouldBe` [(4, 1), (3, 0), (2, 0), (1, 0), (0, 0), (0, 0), (0, 0)]

    it "Should moveTail step 2" $ do
      moveTail (4, 2) [(4, 1), (3, 0), (2, 0), (1, 0), (0, 0), (0, 0), (0, 0)] `shouldBe` [(4, 2), (4, 1), (3, 1), (2, 1), (1, 1), (0, 0), (0, 0)]

    it "Should moveTail step 3" $ do
      moveTail (4, 3) [(4, 2), (4, 1), (3, 1), (2, 1), (1, 1), (0, 0), (0, 0)] `shouldBe` [(4, 3), (4, 2), (3, 1), (2, 1), (1, 1), (0, 0), (0, 0)]

    it "Should moveTail step 4" $ do
      moveTail (4, 4) [(4, 3), (4, 2), (3, 1), (2, 1), (1, 1), (0, 0), (0, 0)] `shouldBe` [(4, 4), (4, 3), (4, 2), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 4" $ do
      moveTail (3, 4) [(4, 4), (4, 3), (4, 2), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(3, 4), (4, 3), (4, 2), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 5" $ do
      moveTail (2, 4) [(3, 4), (4, 3), (4, 2), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(2, 4), (3, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 6" $ do
      moveTail (1, 4) [(2, 4), (3, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(1, 4), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 7" $ do
      moveTail (1, 3) [(1, 4), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(1, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 8" $ do
      moveTail (2, 3) [(1, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(2, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 9" $ do
      moveTail (3, 3) [(2, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(3, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 10" $ do
      moveTail (4, 3) [(3, 3), (2, 4), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(4, 3), (3, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step 11" $ do
      moveTail (5, 3) [(4, 3), (3, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(5, 3), (4, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step D 1 (0)" $ do
      moveTail (5, 2) [(5, 3), (4, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(5, 2), (4, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]

    it "Should moveTail step L 5 (0)" $ do
      moveTail (4, 2) [(5, 2), (4, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)] `shouldBe` [(4, 2), (4, 3), (3, 3), (3, 2), (2, 2), (1, 1), (0, 0)]
