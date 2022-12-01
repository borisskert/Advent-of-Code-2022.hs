module Day01.CalorieCountingSpec (spec) where

import Test.Hspec
import Day01.CalorieCounting (maximumCalories)

exampleInput :: String
exampleInput = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

spec :: Spec
spec = do
  describe "Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?" $ do
        it "In the example above, this is 24000 (carried by the fourth Elf)" $ do
            maximumCalories exampleInput `shouldBe` 24000
