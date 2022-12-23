module Common.RangeSpec (spec) where

import Common.Range
import Test.Hspec
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "When create Range Int with small natural numbers" $ do
    let range = from 1 10 :: Range Int

    it "Should provide start" $ do
      start range `shouldBe` 1

    it "Should provide end" $ do
      end range `shouldBe` 10

    it "Should provide size" $ do
      size range `shouldBe` 10

    describe "When intersect with identical range" $ do
      let intersection = fromJust . (range `intersect`) $ (from 1 10 :: Range Int)

      it "Should provide start" $ do
        start intersection `shouldBe` 1

      it "Should provide end" $ do
        end intersection `shouldBe` 10

      it "Should provide size" $ do
        size intersection `shouldBe` 10

    describe "When intersect with larger overlapping range" $ do
      let intersection = fromJust . (range `intersect`) $ (from (-5) 15 :: Range Int)

      it "Should provide start" $ do
        start intersection `shouldBe` 1

      it "Should provide end" $ do
        end intersection `shouldBe` 10

      it "Should provide size" $ do
        size intersection `shouldBe` 10

    describe "When intersect with overlapping left side" $ do
      let intersection = fromJust . (range `intersect`) $ (from 0 5 :: Range Int)

      it "Should provide start" $ do
        start intersection `shouldBe` 1

      it "Should provide end" $ do
        end intersection `shouldBe` 5

      it "Should provide size" $ do
        size intersection `shouldBe` 5

    describe "When intersect with overlapping left side (opposite)" $ do
      let intersection = fromJust . ((from 0 5 :: Range Int) `intersect`) $ range

      it "Should provide start" $ do
        start intersection `shouldBe` 1

      it "Should provide end" $ do
        end intersection `shouldBe` 5

      it "Should provide size" $ do
        size intersection `shouldBe` 5

    describe "When intersect with overlapping right side" $ do
      let intersection = fromJust . (range `intersect`) $ (from 7 12 :: Range Int)

      it "Should provide start" $ do
        start intersection `shouldBe` 7

      it "Should provide end" $ do
        end intersection `shouldBe` 10

      it "Should provide size" $ do
        size intersection `shouldBe` 4

    describe "When intersect with overlapping right side" $ do
      let intersection = fromJust . ((from 7 12 :: Range Int) `intersect`) $ range

      it "Should provide start" $ do
        start intersection `shouldBe` 7

      it "Should provide end" $ do
        end intersection `shouldBe` 10

      it "Should provide size" $ do
        size intersection `shouldBe` 4

    describe "When intersect with overlapping midd" $ do
      let intersection = fromJust . (range `intersect`) $ (from 4 7 :: Range Int)

      it "Should provide start" $ do
        start intersection `shouldBe` 4

      it "Should provide end" $ do
        end intersection `shouldBe` 7

      it "Should provide size" $ do
        size intersection `shouldBe` 4

-- | -------------------------------------------------------------------------------------------------------------------
-- | Union tests
-- | -------------------------------------------------------------------------------------------------------------------

    describe "When union with adjacent range (tailing)" $ do
      let unioned = from 11 20 `union` range

      it "Should provide start" $ do
        start unioned `shouldBe` 1

      it "Should provide end" $ do
        end unioned `shouldBe` 20

      it "Should provide size" $ do
        size unioned `shouldBe` 20

    describe "When union with adjacent range (tailing, but opposite direction)" $ do
      let unioned = range `union` from 11 20

      it "Should provide start" $ do
        start unioned `shouldBe` 1

      it "Should provide end" $ do
        end unioned `shouldBe` 20

      it "Should provide size" $ do
        size unioned `shouldBe` 20

    describe "When union with overlapping range" $ do
      let unioned = from 5 15 `union` range

      it "Should provide start" $ do
        start unioned `shouldBe` 1

      it "Should provide end" $ do
        end unioned `shouldBe` 15

      it "Should provide size" $ do
        size unioned `shouldBe` 15

    describe "When union with overlapping range (opposite)" $ do
      let unioned = from 5 15 `union` range

      it "Should provide start" $ do
        start unioned `shouldBe` 1

      it "Should provide end" $ do
        end unioned `shouldBe` 15

      it "Should provide size" $ do
        size unioned `shouldBe` 15

    describe "When union with non-adjaent range" $ do
      let unioned = range `union` from 13 20

      it "Should result" $ do
        unioned `shouldBe` fromJust (from 11 12 `excludeFrom` from 1 20)

      it "Should provide start" $ do
        start unioned `shouldBe` 1

      it "Should provide end" $ do
        end unioned `shouldBe` 20

      it "Should provide size" $ do
        size unioned `shouldBe` 18

-- | -------------------------------------------------------------------------------------------------------------------
-- | Exclusion tests
-- | -------------------------------------------------------------------------------------------------------------------

    describe "When exclude identical range"$ do
      let excluded = range `excludeFrom` from 1 10

      it "Should provide Nothing" $ do
        excluded `shouldBe` Nothing

    describe "When exclude leading gap" $ do
      let range' = fromJust . excludeFrom (from 1 3) $ range

      it "Should provide start" $ do
        start range' `shouldBe` 4

      it "Should provide end" $ do
        end range' `shouldBe` 10

      it "Should provide size" $ do
        size range' `shouldBe` 7

    describe "When exclude trailing gap" $ do
      let range' = fromJust . excludeFrom (from 8 10) $ range

      it "Should provide start" $ do
        start range' `shouldBe` 1

      it "Should provide end" $ do
        end range' `shouldBe` 7

      it "Should provide size" $ do
        size range' `shouldBe` 7

    describe "When exclude a smaller range which is within" $ do
      let range' = fromJust . excludeFrom (from 2 3) $ range

      it "Should still provide start" $ do
        start range' `shouldBe` 1

      it "Should still provide end" $ do
        end range' `shouldBe` 10

      it "Should provide size" $ do
        size range' `shouldBe` 8

      describe "When exclude a smaller range which is within and overlaps" $ do
        let range'' = fromJust .excludeFrom (from 3 4) $ range'

        it "Should still provide start" $ do
          start range'' `shouldBe` 1

        it "Should still provide end" $ do
          end range'' `shouldBe` 10

        it "Should provide size" $ do
          size range'' `shouldBe` 7

        describe "When intersect with a smaller range which is within and overlaps" $ do
          let range''' = fromJust . intersect (from 3 7) $ range''

          it "Should still provide start" $ do
            start range''' `shouldBe` 5

          it "Should still provide end" $ do
            end range''' `shouldBe` 7

          it "Should provide size" $ do
            size range''' `shouldBe` 3

        describe "When intersect with a smaller range which is within and overlaps (opposite direction)" $ do
          let range''' = fromJust . intersect range'' $ from 3 7

          it "Should still provide start" $ do
            start range''' `shouldBe` 5

          it "Should still provide end" $ do
            end range''' `shouldBe` 7

          it "Should provide size" $ do
            size range''' `shouldBe` 3

        describe "When exclude a smaller range which is within and overlaps and is the same" $ do
          let range''' = fromJust .excludeFrom (from 3 4) $ range''

          it "Should still provide start" $ do
            start range''' `shouldBe` 1

          it "Should still provide end" $ do
            end range''' `shouldBe` 10

          it "Should provide size" $ do
            size range''' `shouldBe` 7

  describe "When create Range Int with negative numbers" $ do
    let range = from (-10) (-1) :: Range Int

    it "Should provide start" $ do
      start range `shouldBe` (-10)

    it "Should provide end" $ do
      end range `shouldBe` (-1)

    it "Should provide size" $ do
      size range `shouldBe` 10

  describe "When create Range Int with mixed numbers" $ do
    let range = from (-10) 10 :: Range Int

    it "Should provide start" $ do
      start range `shouldBe` (-10)

    it "Should provide end" $ do
      end range `shouldBe` 10

    it "Should provide size" $ do
      size range `shouldBe` 21

-- | -------------------------------------------------------------------------------------------------------------------
-- | Simple tests
-- | -------------------------------------------------------------------------------------------------------------------

  describe "When test size" $ do
    it "Should [2-2] => 1" $ do
      size (from 2 2) `shouldBe` (1 :: Int)

    it "Should [0-4] => 5" $ do
      size (from 0 4) `shouldBe` (5 :: Int)

  describe "When exclude something" $ do
    let excluded = fromJust . excludeFrom (from 2 2) $ from 0 4
    it "should [2 2] `excludeFrom` [0 4] => [0-4] w/o [2-2]" $ do
      size excluded `shouldBe` (4 :: Int)
      start excluded `shouldBe` 0
      end excluded `shouldBe` 4

    it "should [2 4] `excludeFrom` [3 7] => [5 7]" $ do
      from (2:: Int) 4 `excludeFrom` from 3 7 `shouldBe` Just (from 5 7)

    it "should do something" $ do
      from (0:: Int) 5 `excludeFrom` from 1 10 `shouldBe` Just (from 6 10)

  describe "When intersect something" $ do
    it "should [2 4] `intersect` [3 7] => [3 4]" $ do
      from (2:: Int) 4 `intersect` from 3 7 `shouldBe` Just (from 3 4)

    it "should do something" $ do
      from (0:: Int) 5 `intersect` from 1 10 `shouldBe` Just (from 1 5)

  describe "When test isOverlappingWith" $ do
    it "[2-4] overlaps with [3-7]" $ do
      from (2:: Int) 4 `isOverlappingWith` from 3 7 `shouldBe` True

    it "[3-7] overlaps with [2-4]" $ do
      from (3:: Int) 7 `isOverlappingWith` from 2 4  `shouldBe` True

    it "[0-5] overlaps with [1-10]" $ do
      from (0:: Int) 5 `isOverlappingWith` from 1 10 `shouldBe` True

    it "[1-10] overlaps with [0-5]" $ do
      from (1:: Int) 10`isOverlappingWith` from 0 5  `shouldBe` True

    it "[0-5] does NOT overlap with [6-10]" $ do
      from (0:: Int) 5 `isOverlappingWith` from 6 10 `shouldBe` False

    it "[6-10] does NOT overlap with [0-5]" $ do
      from (6 :: Int) 10 `isOverlappingWith` from 0 5  `shouldBe` False

  describe "When test union" $ do
    it "[0-2] `union` [0-2] => [0-2]" $ do
      from (0 :: Int) 2 `union` from 0 2 `shouldBe` from 0 2

    it "[0-2] `union` [0-4] => [0-4]" $ do
      from (0 :: Int) 2 `union` from 0 4 `shouldBe` from 0 4

    it "[0-4] `union` [0-2] => [0-4]" $ do
      from (0 :: Int) 4 `union` from 0 2 `shouldBe` from 0 4

    it "[0-2] `union` [3-4] => [0-4]" $ do
      from (0 :: Int) 2 `union` from 3 4 `shouldBe` from 0 4

    it "[3-4] `union` [0-2] => [0-4]" $ do
      from (3 :: Int) 4 `union` from 0 2 `shouldBe` from 0 4

    it "[0-3] `union` [3-4] => [0-4]" $ do
      from (0 :: Int) 3 `union` from 3 4 `shouldBe` from 0 4

    it "[3-4] `union` [0-3] => [0-4]" $ do
      from (3 :: Int) 4 `union` from 0 3 `shouldBe` from 0 4

    it "[0-1] `union` [2-4] => [0-4]" $ do
      from (0 :: Int) 1 `union` from 2 4 `shouldBe` from 0 4

    it "[2-4] `union` [0-1] => [0-4]" $ do
      from (2 :: Int) 4 `union` from 0 1 `shouldBe` from 0 4

    it "[0-1] `union` [3-4] => [0-4] w/o [2-2]" $ do
      from (0 :: Int) 1 `union` from 3 4 `shouldBe` fromJust (from 2 2 `excludeFrom` from 0 4)
