module Common.Common.OctaGridSpec (spec) where

import Common.OctaGrid
import Test.Hspec

spec :: Spec
spec = do
  describe "When searching for adjacent Positions" $ do
    it "Should (2, 2) NOT be adjacent to (1, 0)" $ do
      areAdjacent (2, 2) (1, 0) `shouldBe` False

    it "Should (1, 0) NOT be adjacent to (2, 2)" $ do
      areAdjacent (1, 0) (2, 2) `shouldBe` False
