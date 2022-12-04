module Day04.CampCleanupSpec (spec) where

import Day04.CampCleanup
import Test.Hspec

exampleInput :: String
exampleInput = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

spec :: Spec
spec = do
  describe "In how many assignment pairs does one range fully contain the other?" $ do
    it "In this example, there are 2 such pairs" $ do
      howManyPairsFullyContainAnother exampleInput `shouldBe` 2
