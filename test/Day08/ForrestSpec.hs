module Day08.ForrestSpec (spec) where

import Day08.Forrest
import Test.Hspec

exampleInput :: String
exampleInput = "30373\n25512\n65332\n33549\n35390"

exampleForrest :: Forrest
exampleForrest = read exampleInput

spec :: Spec
spec = do
  describe "When look at the Forrest" $ do
    it "Should show visible trees from North" $ do
      visibleFromNorth exampleForrest
        `shouldBe` [ [tree (0, 0) 3, tree (0, 2) 6],
                     [tree (1, 0) 0, tree (1, 1) 5],
                     [tree (2, 0) 3, tree (2, 1) 5],
                     [tree (3, 0) 7, tree (3, 4) 9],
                     [tree (4, 0) 3, tree (4, 3) 9]
                   ]

      visibleFromWest exampleForrest
        `shouldBe` [ [tree (0, 0) 3, tree (3, 0) 7],
                     [tree (0, 1) 2, tree (1, 1) 5],
                     [tree (0, 2) 6],
                     [tree (0, 3) 3, tree (2, 3) 5, tree (4, 3) 9],
                     [tree (0, 4) 3, tree (1, 4) 5, tree (3, 4) 9]
                   ]

      visibleFromSouth exampleForrest
        `shouldBe` [ [tree (0, 4) 3, tree (0, 2) 6],
                     [tree (1, 4) 5],
                     [tree (2, 4) 3, tree (2, 3) 5],
                     [tree (3, 4) 9],
                     [tree (4, 4) 0, tree (4, 3) 9]
                   ]

      visibleFromEast exampleForrest
        `shouldBe` [ [tree (4, 0) 3, tree (3, 0) 7],
                     [tree (4, 1) 2, tree (2, 1) 5],
                     [tree (4, 2) 2, tree (3, 2) 3, tree (1, 2) 5, tree (0, 2) 6],
                     [tree (4, 3) 9],
                     [tree (4, 4) 0, tree (3, 4) 9]
                   ]

  describe "When determining scenicScore of Trees" $ do
    it "For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2)." $ do
      scenicScore (tree (2, 1) 5) exampleForrest `shouldBe` 4

    it "This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house." $ do
      scenicScore (tree (2, 3) 5) exampleForrest `shouldBe` 8
