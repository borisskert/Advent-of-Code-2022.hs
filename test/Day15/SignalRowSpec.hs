module Day15.SignalRowSpec (spec) where

import qualified Common.OctaGridPosition as Position (from)
import Day15.SignalRow
import Test.Hspec

spec :: Spec
spec = do
  describe "When create example SignalRow at row 11" $ do
    let signalRow = fromList [Position.from (-3) 11, Position.from (-2) 11, Position.from (-1) 11, Position.from 0 11, Position.from 1 11, Position.from 2 11, Position.from 3 11, Position.from 4 11, Position.from 5 11, Position.from 6 11, Position.from 7 11, Position.from 8 11, Position.from 9 11, Position.from 10 11, Position.from 11 11, Position.from 12 11, Position.from 13 11, Position.from 15 11, Position.from 16 11, Position.from 17 11, Position.from 18 11, Position.from 19 11, Position.from 20 11, Position.from 21 11, Position.from 22 11, Position.from 23 11, Position.from 24 11, Position.from 25 11]

    it "Should find hole at position 14 11" $ do
      hole signalRow `shouldBe` Just (Position.from 14 11)

  describe "When create example SignalRow at row 10" $ do
    let signalRow = fromList [Position.from (-2) 10, Position.from (-1) 10, Position.from 0 10, Position.from 1 10, Position.from 2 10, Position.from 3 10, Position.from 4 10, Position.from 5 10, Position.from 6 10, Position.from 7 10, Position.from 8 10, Position.from 9 10, Position.from 10 10, Position.from 11 10, Position.from 12 10, Position.from 13 10, Position.from 14 10, Position.from 15 10, Position.from 16 10, Position.from 17 10, Position.from 18 10, Position.from 19 10, Position.from 20 10, Position.from 21 10, Position.from 22 10, Position.from 23 10, Position.from 24 10]

    it "Should not find hole" $ do
      hole signalRow `shouldBe` Nothing
