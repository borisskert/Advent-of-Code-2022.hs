module Common.OctaGridPositionSpec (spec) where

import Common.Grid
import Common.OctaGridPosition
import qualified Data.Set as Set (fromList)
import Test.Hspec

spec :: Spec
spec = do
  describe "When searching for adjacent Positions" $ do
    it "Should (2, 2) NOT be adjacent to (1, 0)" $ do
      areAdjacent (fromTuple (2, 2)) (fromTuple (1, 0)) `shouldBe` False

    it "Should (1, 0) NOT be adjacent to (2, 2)" $ do
      areAdjacent (fromTuple (1, 0)) (fromTuple (2, 2)) `shouldBe` False

  describe "When determining positions within certain manhattan distance" $ do
    it "Should return position only for 0" $ do
      withinManhattanDistance 0 (from 0 0) `shouldBe` Set.fromList [from 0 0]

    it "Should return positions for size 1" $ do
      withinManhattanDistance 1 (from 0 0) `shouldBe` Set.fromList [from (-1) 0, from 0 (-1), from 0 0, from 1 0, from 0 1]

    it "Should return positions for size 2" $ do
      withinManhattanDistance 2 (from 0 0) `shouldBe` Set.fromList [from (-2) 0, from (-1) (-1), from (-1) 0, from (-1) 1, from 0 (-2), from 0 (-1), from 0 0, from 0 1, from 0 2, from 1 (-1), from 1 0, from 1 1, from 2 0]

    it "Should return positions for size 3" $ do
      withinManhattanDistance 3 (from 0 0) `shouldBe` Set.fromList [from (-3) 0, from (-2) (-1), from (-2) 0, from (-2) 1, from (-1) (-2), from (-1) (-1), from (-1) 0, from (-1) 1, from (-1) 2, from 0 (-3), from 0 (-2), from 0 (-1), from 0 0, from 0 1, from 0 2, from 0 3, from 1 (-2), from 1 (-1), from 1 0, from 1 1, from 1 2, from 2 (-1), from 2 0, from 2 1, from 3 0]

    it "Should return positions for size 4" $ do
      withinManhattanDistance 4 (from 0 0) `shouldBe` Set.fromList [from (-4) 0, from (-3) (-1), from (-3) 0, from (-3) 1, from (-2) (-2), from (-2) (-1), from (-2) 0, from (-2) 1, from (-2) 2, from (-1) (-3), from (-1) (-2), from (-1) (-1), from (-1) 0, from (-1) 1, from (-1) 2, from (-1) 3, from 0 (-4), from 0 (-3), from 0 (-2), from 0 (-1), from 0 0, from 0 1, from 0 2, from 0 3, from 0 4, from 1 (-3), from 1 (-2), from 1 (-1), from 1 0, from 1 1, from 1 2, from 1 3, from 2 (-2), from 2 (-1), from 2 0, from 2 1, from 2 2, from 3 (-1), from 3 0, from 3 1, from 4 0]

    it "Should return positions for size 3 at (4, 0)" $ do
      withinManhattanDistance 3 (from 4 0) `shouldBe` Set.fromList [from 1 0, from 2 (-1), from 2 0, from 2 1, from 3 (-2), from 3 (-1), from 3 0, from 3 1, from 3 2, from 4 (-3), from 4 (-2), from 4 (-1), from 4 0, from 4 1, from 4 2, from 4 3, from 5 (-2), from 5 (-1), from 5 0, from 5 1, from 5 2, from 6 (-1), from 6 0, from 6 1, from 7 0]

    it "Should return positions for size 3 at (0, 3)" $ do
      withinManhattanDistance 3 (from 0 3)
        `shouldBe` Set.fromList
          [ from (-3) 3,
            from (-2) 2,
            from (-2) 3,
            from (-2) 4,
            from (-1) 1,
            from (-1) 2,
            from (-1) 3,
            from (-1) 4,
            from (-1) 5,
            from 0 0,
            from 0 1,
            from 0 2,
            from 0 3,
            from 0 4,
            from 0 5,
            from 0 6,
            from 1 1,
            from 1 2,
            from 1 3,
            from 1 4,
            from 1 5,
            from 2 2,
            from 2 3,
            from 2 4,
            from 3 3
          ]

    it "Should return positions for size 3 at (4, 3)" $ do
      withinManhattanDistance 3 (from 4 3)
        `shouldBe` Set.fromList
          [ from 1 3,
            from 2 2,
            from 2 3,
            from 2 4,
            from 3 1,
            from 3 2,
            from 3 3,
            from 3 4,
            from 3 5,
            from 4 0,
            from 4 1,
            from 4 2,
            from 4 3,
            from 4 4,
            from 4 5,
            from 4 6,
            from 5 1,
            from 5 2,
            from 5 3,
            from 5 4,
            from 5 5,
            from 6 2,
            from 6 3,
            from 6 4,
            from 7 3
          ]
