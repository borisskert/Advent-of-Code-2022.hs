module Day15.SensorAndBeaconSpec (spec) where

import qualified Common.Grid as Grid
import qualified Common.OctaGridPosition as Position
import Day15.SensorAndBeacon
import Test.Hspec

spec :: Spec
spec = do
  describe "When read from String" $ do
    it "Should read first example" $ do
      read "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `shouldBe` from (Position.from 2 18) (Position.from (-2) 15)
