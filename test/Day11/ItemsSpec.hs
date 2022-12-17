module Day11.ItemsSpec (spec) where

import Day11.Item
import Day11.Items
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading Items from Monkey's second line" $ do
    it "Should read first example" $ do
      read "Starting items: 79, 98" `shouldBe` fromList [from 79, from 98]

    it "Should read second example" $ do
      read "  Starting items: 54, 65, 75, 74" `shouldBe` fromList [from 54, from 65, from 75, from 74]
