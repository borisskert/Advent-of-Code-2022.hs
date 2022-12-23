module Day15.ScannerAreaSpec (spec) where

import qualified Common.OctaGridPosition as Position (from)
import qualified Data.Set as Set (fromList)
import Day15.ScannerArea
import qualified Day15.SignalRow as SignalRow (from)
import Test.Hspec

spec :: Spec
spec = do
  describe "When create ScannerArea" $ do
    let area = from (Position.from 8 7) (Position.from 2 10)

    it "should determine if ScannerArea is intersecting with row -3, which is False" $ do
      intersectsRow (-3) area `shouldBe` False

    it "should determine if ScannerArea is intersecting with row -2, which is true" $ do
      intersectsRow (-2) area `shouldBe` True

    it "should determine if ScannerArea is intersecting with row -1, which is True" $ do
      intersectsRow (-1) area `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 8, which is true" $ do
      intersectsRow 8 area `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 10, which is true" $ do
      intersectsRow 10 area `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 16, which is false" $ do
      intersectsRow 16 area `shouldBe` True

    it "should determine if ScannerArea is intersecting with row 17, which is false" $ do
      intersectsRow 17 area `shouldBe` False

    it "Should extract intersecting positions with row" $ do
      intersectionRow 10 area
        `shouldBe` SignalRow.from 3 14 10
