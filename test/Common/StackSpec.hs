module Common.StackSpec (spec) where

import Common.Stack
import Test.Hspec

emptyStack :: Stack Char
emptyStack = empty

oneItemStack :: Stack Char
oneItemStack = push 'a' empty

twoItemStack :: Stack Char
twoItemStack = push 'b' . push 'a' $ empty

spec :: Spec
spec = do
  describe "Empty Stack" $ do
    it "Should create" $ do
      emptyStack `shouldBe` fromList []

    it "Should be empty" $ do
      isEmpty emptyStack `shouldBe` True

  describe "One item Stack" $ do
    it "Should create" $ do
      oneItemStack `shouldBe` fromList ['a']

    it "Should not be empty" $ do
      isEmpty oneItemStack `shouldBe` False

    it "Should provide top item" $ do
      top oneItemStack `shouldBe` 'a'

    it "Should be empty after pop" $ do
      (isEmpty . pop $ oneItemStack) `shouldBe` True

  describe "Two item Stack" $ do
    it "Should create" $ do
      twoItemStack `shouldBe` fromList ['b', 'a']

    it "Should not be empty" $ do
      isEmpty twoItemStack `shouldBe` False

    it "Should provide top item" $ do
      top twoItemStack `shouldBe` 'b'

    it "Should NOT be empty after pop" $ do
      (isEmpty . pop $ twoItemStack) `shouldBe` False

    it "Should provide first item after pop" $ do
      (top . pop $ twoItemStack) `shouldBe` 'a'

    it "Should be empty after second pop" $ do
      (isEmpty . pop . pop $ twoItemStack) `shouldBe` True
