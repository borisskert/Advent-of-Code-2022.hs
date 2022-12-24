module Day15.BeaconExclusionZoneSpec (spec) where

import Day15.BeaconExclusionZone
import Test.Hspec

exampleInput :: String
exampleInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

spec :: Spec
spec = do
--  describe "Consult the report from the sensors you just deployed. In the row where y=2000000, how many positions cannot contain a beacon?" $ do
--    it "In this example, in the row where y=10, there are 26 positions where a beacon cannot be present." $ do
--      howManyPositions 10 exampleInput `shouldBe` 26

  describe "Find the only possible position for the distress beacon. What is its tuning frequency?" $ do
    it "With this reduced search area, there is only a single position that could have a beacon: x=14, y=11. The tuning frequency for this distress beacon is 56000011." $ do
      tuningFrequency 10 exampleInput `shouldBe` 56000011
