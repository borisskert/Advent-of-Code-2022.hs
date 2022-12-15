module Day10.DeviceSpec (spec) where

import Day10.Device
import Day10.Program
import Test.Hspec

exampleInput :: String
exampleInput = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"

spec :: Spec
spec = do
  describe "When Running example program" $ do
    let program = read exampleInput :: Program

    it "During the 20th cycle, register X has the value 21, so the signal strength is 20 * 21 = 420" $ do
      (signalStrength . ticks 20 . load program $ crtDevice) `shouldBe` 420

    it "During the 60th cycle, register X has the value 19, so the signal strength is 60 * 19 = 1140." $ do
      (signalStrength . ticks 60 . load program $ crtDevice) `shouldBe` 1140

    it "During the 100th cycle, register X has the value 18, so the signal strength is 100 * 18 = 1800." $ do
      (signalStrength . ticks 100 . load program $ crtDevice) `shouldBe` 1800

    it "During the 140th cycle, register X has the value 21, so the signal strength is 140 * 21 = 2940." $ do
      (signalStrength . ticks 140 . load program $ crtDevice) `shouldBe` 2940

    it "During the 180th cycle, register X has the value 16, so the signal strength is 180 * 16 = 2880." $ do
      (signalStrength . ticks 180 . load program $ crtDevice) `shouldBe` 2880

    it "During the 220th cycle, register X has the value 18, so the signal strength is 220 * 18 = 3960." $ do
      (signalStrength . ticks 220 . load program $ crtDevice) `shouldBe` 3960
