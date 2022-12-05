module Day05.MoveSpec (spec) where

import Day05.Move
import Test.Hspec

spec :: Spec
spec = do
  describe "Should read Moves" $ do
    it "Should read first from example" $ do
      readOne "move 1 from 2 to 3" `shouldBe` moveOf 1 '2' '3'

    it "Should read example with two-digit numbers" $ do
      readOne "move 32 from 2 to 3" `shouldBe` moveOf 32 '2' '3'

    it "Should read example with three-digit numbers" $ do
      readOne "move 584 from 9 to 4" `shouldBe` moveOf 584 '9' '4'
