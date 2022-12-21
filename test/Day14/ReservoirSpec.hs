module Day14.ReservoirSpec (spec) where

import Common.Grid (fromTuple)
import Day14.Material
import Day14.Reservoir (empty, insertScans, sandUnits, toList, withGround)
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When inserting RockScans" $ do
    let reservoir = insertScans (read exampleInput) empty

    it "Should insert example" $ do
      toList reservoir
        `shouldBe` [ (fromTuple (494, 9), rock),
                     (fromTuple (495, 9), rock),
                     (fromTuple (496, 6), rock),
                     (fromTuple (496, 9), rock),
                     (fromTuple (497, 6), rock),
                     (fromTuple (497, 9), rock),
                     (fromTuple (498, 4), rock),
                     (fromTuple (498, 5), rock),
                     (fromTuple (498, 6), rock),
                     (fromTuple (498, 9), rock),
                     (fromTuple (499, 9), rock),
                     (fromTuple (500, 0), sandSource),
                     (fromTuple (500, 9), rock),
                     (fromTuple (501, 9), rock),
                     (fromTuple (502, 4), rock),
                     (fromTuple (502, 5), rock),
                     (fromTuple (502, 6), rock),
                     (fromTuple (502, 7), rock),
                     (fromTuple (502, 8), rock),
                     (fromTuple (502, 9), rock),
                     (fromTuple (503, 4), rock)
                   ]

    it "Should have no sand units" $ do
      sandUnits reservoir `shouldBe` []

    describe "When add ground to reservoir" $ do
      let reservoirWithGround = withGround reservoir

      it "Should have ground in positions" $ do
        toList reservoirWithGround
          `shouldBe` [ (fromTuple (489, 11), rock),
                       (fromTuple (490, 11), rock),
                       (fromTuple (491, 11), rock),
                       (fromTuple (492, 11), rock),
                       (fromTuple (493, 11), rock),
                       (fromTuple (494, 9), rock),
                       (fromTuple (494, 11), rock),
                       (fromTuple (495, 9), rock),
                       (fromTuple (495, 11), rock),
                       (fromTuple (496, 6), rock),
                       (fromTuple (496, 9), rock),
                       (fromTuple (496, 11), rock),
                       (fromTuple (497, 6), rock),
                       (fromTuple (497, 9), rock),
                       (fromTuple (497, 11), rock),
                       (fromTuple (498, 4), rock),
                       (fromTuple (498, 5), rock),
                       (fromTuple (498, 6), rock),
                       (fromTuple (498, 9), rock),
                       (fromTuple (498, 11), rock),
                       (fromTuple (499, 9), rock),
                       (fromTuple (499, 11), rock),
                       (fromTuple (500, 0), sandSource),
                       (fromTuple (500, 9), rock),
                       (fromTuple (500, 11), rock),
                       (fromTuple (501, 9), rock),
                       (fromTuple (501, 11), rock),
                       (fromTuple (502, 4), rock),
                       (fromTuple (502, 5), rock),
                       (fromTuple (502, 6), rock),
                       (fromTuple (502, 7), rock),
                       (fromTuple (502, 8), rock),
                       (fromTuple (502, 9), rock),
                       (fromTuple (502, 11), rock),
                       (fromTuple (503, 4), rock),
                       (fromTuple (503, 11), rock),
                       (fromTuple (504, 11), rock),
                       (fromTuple (505, 11), rock),
                       (fromTuple (506, 11), rock),
                       (fromTuple (507, 11), rock),
                       (fromTuple (508, 11), rock),
                       (fromTuple (509, 11), rock),
                       (fromTuple (510, 11), rock),
                       (fromTuple (511, 11), rock)
                     ]

      it "Should show ground" $ do
        show reservoirWithGround
          `shouldBe` "...........+...........\n.......................\n.......................\n.......................\n.........#...##........\n.........#...#.........\n.......###...#.........\n.............#.........\n.............#.........\n.....#########.........\n.......................\n#######################"
