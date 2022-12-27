module Day15.SensorAndBeaconReportSpec (spec) where

import qualified Common.OctaGridPosition as Position
import Day15.SensorAndBeaconReport
import Test.Hspec

exampleInput :: String
exampleInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

spec :: Spec
spec = do
  describe "When read from String" $ do
    it "Should read first example" $ do
      read "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `shouldBe` from (Position.from 2 18) (Position.from (-2) 15)

    it "Should read second example" $ do
      read "Sensor at x=9, y=16: closest beacon is at x=10, y=16" `shouldBe` from (Position.from 9 16) (Position.from 10 16)

  describe "When readMany from String" $ do
    it "Should read first example" $ do
      readMany "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `shouldBe` [from (Position.from 2 18) (Position.from (-2) 15)]

    it "Should read second example" $ do
      readMany "Sensor at x=9, y=16: closest beacon is at x=10, y=16" `shouldBe` [from (Position.from 9 16) (Position.from 10 16)]

    it "Should read multiple examples" $ do
      readMany exampleInput
        `shouldBe` [ from (Position.from 2 18) (Position.from (-2) 15),
                     from (Position.from 9 16) (Position.from 10 16),
                     from (Position.from 13 2) (Position.from 15 3),
                     from (Position.from 12 14) (Position.from 10 16),
                     from (Position.from 10 20) (Position.from 10 16),
                     from (Position.from 14 17) (Position.from 10 16),
                     from (Position.from 8 7) (Position.from 2 10),
                     from (Position.from 2 0) (Position.from 2 10),
                     from (Position.from 0 11) (Position.from 2 10),
                     from (Position.from 20 14) (Position.from 25 17),
                     from (Position.from 17 20) (Position.from 21 22),
                     from (Position.from 16 7) (Position.from 15 3),
                     from (Position.from 14 3) (Position.from 15 3),
                     from (Position.from 20 1) (Position.from 15 3)
                   ]
