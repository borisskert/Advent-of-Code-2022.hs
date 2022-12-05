module Day05.StacksOfCratesSpec (spec) where

import Day05.Crate
import Day05.StacksOfCrates
import Test.Hspec

exampleInput :: String
exampleInput = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 "

spec :: Spec
spec = do
  describe "Should read from String" $ do
    it "Should read example" $ do
      readFrom exampleInput
        `shouldBe` ( pushAt '3' (crateOf 'P')
                       . pushAt '2' (crateOf 'D')
                       . pushAt '2' (crateOf 'C')
                       . pushAt '2' (crateOf 'M')
                       . pushAt '1' (crateOf 'N')
                       . pushAt '1' (crateOf 'Z')
                       $ empty
                   )
