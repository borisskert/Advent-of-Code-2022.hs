module Day08.ForrestSpec (spec) where

import Day08.Forrest
import Test.Hspec

exampleInput :: String
exampleInput = "30373\n25512\n65332\n33549\n35390"

exampleForrest :: Forrest
exampleForrest =
  fromList
    [ [3, 0, 3, 7, 3],
      [2, 5, 5, 1, 2],
      [6, 5, 3, 3, 2],
      [3, 3, 5, 4, 9],
      [3, 5, 3, 9, 0]
    ]

spec :: Spec
spec = do
  describe "When readFrom lines" $ do
    it "Should readFrom exampleInput" $ do
      readFrom exampleInput `shouldBe` exampleForrest

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
