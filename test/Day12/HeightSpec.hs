module Day12.HeightSpec (spec) where

import Test.Hspec
import Day12.Height

spec :: Spec
spec = do
  describe "When check passes Heights" $ do
    it "Should pass from 'S' to 'a'" $ do
      arePassable start (fromChar 'a') `shouldBe` True

    it "Should pass from 'S' to 'b'" $ do
      arePassable start (fromChar 'b') `shouldBe` True

    it "Should pass from 'a' to 'a'" $ do
      arePassable  (fromChar 'a')  (fromChar 'a') `shouldBe` True

    it "Should pass from 'a' to 'b'" $ do
      arePassable  (fromChar 'a') (fromChar 'b') `shouldBe` True

    it "Should pass from 'b' to 'a'" $ do
      arePassable  (fromChar 'b') (fromChar 'a') `shouldBe` True

    it "Should NOT pass from 'a' to 'c'" $ do
      arePassable  (fromChar 'a') (fromChar 'c') `shouldBe` False

    it "Should pass from 'z' to 'E'" $ do
      arePassable  (fromChar 'z') end `shouldBe` True

    it "Should pass from 'y' to 'z'" $ do
      arePassable  (fromChar 'y') (fromChar 'z')  `shouldBe` True

    it "Should pass from 'y' to 'E'" $ do
      arePassable  (fromChar 'y') end `shouldBe` True
