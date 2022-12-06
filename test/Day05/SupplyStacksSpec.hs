module Day05.SupplyStacksSpec (spec) where

import Day05.SupplyStacks
import Test.Hspec

exampleInput :: String
exampleInput = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"

spec :: Spec
spec = do
  describe "After the rearrangement procedure completes, what crate ends up on top of each stack?" $ do
    it "in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ" $ do
      topOfEachStack exampleInput `shouldBe` "CMZ"

    it "In this example, the CrateMover 9001 has put the crates in a totally different order: MCD" $ do
      topOfEachStackCrateMover9001 exampleInput `shouldBe` "MCD"
