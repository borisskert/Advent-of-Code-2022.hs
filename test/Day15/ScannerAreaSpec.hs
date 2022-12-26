module Day15.ScannerAreaSpec (spec) where

import qualified Common.OctaGridPosition as Position (from)
import Data.Maybe (isJust)
import Day15.ScannerArea
import qualified Day15.SignalRow as SignalRow (from, withBeacon)
import Test.Hspec

spec :: Spec
spec = do
  describe "When create ScannerArea" $ do
    let area = from (Position.from 8 7) (Position.from 2 10)

    it "should determine if ScannerArea is intersecting with row -3, which is False" $ do
      (isJust . rowAt (-3) $ area) `shouldBe` False

    it "should determine if ScannerArea is intersecting with row -2, which is true" $ do
      (isJust . rowAt (-2) $ area) `shouldBe` True

    it "should determine if ScannerArea is intersecting with row -1, which is True" $ do
      (isJust . rowAt (-1) $ area) `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 8, which is true" $ do
      (isJust . rowAt 8 $ area) `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 10, which is true" $ do
      (isJust . rowAt 10 $ area) `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 16, which is false" $ do
      (isJust . rowAt 16 $ area) `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 17, which is false" $ do
      (isJust . rowAt 17 $ area) `shouldBe` False

    it "Should extract intersecting positions with row" $ do
      rowAt 10 area
        `shouldBe` Just (SignalRow.withBeacon 2 (SignalRow.from 2 14 10))
