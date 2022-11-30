module LibSpec (spec) where

import Lib (greeting)
import Test.Hspec

spec :: Spec
spec = do
  it "should greet" $ do
    greeting `shouldBe` "Hello Advent-Of-Code-2022!"
