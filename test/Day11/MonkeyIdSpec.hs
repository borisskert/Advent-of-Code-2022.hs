module Day11.MonkeyIdSpec (spec) where

import Day11.MonkeyId
import Test.Hspec

spec :: Spec
spec = do
  describe "When read MonkeyId" $ do
    it "Should read from Monkey head line" $ do
      read "Monkey 0:" `shouldBe` monkeyId 0

    it "Should read from Monkey's `throw` line" $ do
      read "throw to monkey 3" `shouldBe` monkeyId 3
