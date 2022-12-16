module Day11.ItemsSpec (spec) where

import Day11.Item
import Day11.Items
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading Items from Monkey's second line" $ do
    it "Should read first example" $ do
      read "Starting items: 79, 98" `shouldBe` fromList [item 79, item 98]

    it "Should read second example" $ do
      read "  Starting items: 54, 65, 75, 74" `shouldBe` fromList [item 54, item 65, item 75, item 74]
