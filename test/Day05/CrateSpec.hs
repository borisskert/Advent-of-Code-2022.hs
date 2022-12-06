module Day05.CrateSpec (spec) where

import Day05.Crate
import Test.Hspec

spec :: Spec
spec = do
  describe "Should read crates from input" $ do
    it "Should read last line from example" $ do
      readLine "[Z] [M] [P]" `shouldBe` [Just (crateOf 'Z'), Just (crateOf 'M'), Just (crateOf 'P')]

    it "Should read middle line from example" $ do
      readLine "[N] [C]    " `shouldBe` [Just (crateOf 'N'), Just (crateOf 'C'), Nothing]

    it "Should read first line from example" $ do
      readLine "    [D]    " `shouldBe` [Nothing, Just (crateOf 'D'), Nothing]
