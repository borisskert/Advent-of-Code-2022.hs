module Common.MaybeSpec (spec) where

import Common.Maybe
import Test.Hspec

-- | Test suite for Common.Maybe: adds to numbers when they are not equal
maybeAdd :: (Num a, Eq a) => a -> a -> Maybe a
maybeAdd a b
  | a == b = Nothing
  | otherwise = Just (a + b)

spec :: Spec
spec = do
  describe "When maybeDo something" $ do
    it "Should do" $ do
      maybeDo (+) (Just 5) 5 `shouldBe` (10 :: Int)

    it "Should NOT do" $ do
      maybeDo (+) Nothing 5 `shouldBe` (5 :: Int)

  describe "When replace something" $ do
    it "Should replace" $ do
      replace (maybeAdd 5) 5 `shouldBe` (5 :: Int)

    it "Should NOT replace" $ do
      replace (maybeAdd 10) 7 `shouldBe` (17 :: Int)

  describe "When maybeReplace something" $ do
    it "Should replace because function returns Nothing" $ do
      maybeReplace maybeAdd (Just 5) 5 `shouldBe` (5 :: Int)

    it "Should replace because operand is Nothing" $ do
      maybeReplace maybeAdd Nothing 5 `shouldBe` (5 :: Int)

    it "Should NOT replace: returns sum" $ do
      maybeReplace maybeAdd (Just 5) 7 `shouldBe` (12 :: Int)

  describe "When maybeNothing something" $ do
    it "Should return Nothing because function returns Nothing" $ do
      maybeNothing maybeAdd (Just 5) 5 `shouldBe` (Nothing :: Maybe Int)

    it "Should return Nothing because operand is Nothing" $ do
      maybeNothing maybeAdd Nothing 5 `shouldBe` (Nothing :: Maybe Int)

    it "Should do: returns sum of 5 and 7" $ do
      maybeNothing maybeAdd (Just 5) 7 `shouldBe` (Just 12 :: Maybe Int)

  describe "When maybeSkip something" $ do
    it "Should return Nothing because function returns Nothing" $ do
      maybeSkip maybeAdd (Just 5) 5 `shouldBe` (Nothing :: Maybe Int)

    it "Should skip because operand is Nothing" $ do
      maybeSkip maybeAdd Nothing 5 `shouldBe` (Just 5 :: Maybe Int)

    it "Should NOT skip: returns sum of 5 and 7" $ do
      maybeSkip maybeAdd (Just 5) 7 `shouldBe` (Just 12 :: Maybe Int)
