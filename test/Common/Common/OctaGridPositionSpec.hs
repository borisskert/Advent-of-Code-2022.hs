module Common.Common.OctaGridPositionSpec (spec) where

import Common.OctaGridPosition
import Test.Hspec

spec :: Spec
spec = do
  describe "When searching for adjacent Positions" $ do
    it "Should (2, 2) NOT be adjacent to (1, 0)" $ do
      areAdjacent (fromTuple (2, 2)) (fromTuple (1, 0)) `shouldBe` False

    it "Should (1, 0) NOT be adjacent to (2, 2)" $ do
      areAdjacent (fromTuple (1, 0)) (fromTuple (2, 2)) `shouldBe` False
