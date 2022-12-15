module Common.GridSpec (spec) where

import Common.Grid (height, width)
import Common.GridSpec.TestGrid
import Test.Hspec
import Prelude hiding (lookup)

spec :: Spec
spec = do
  describe "When read Grid from lines" $ do
    it "Should read empty Grid" $ do
      fromLines "" `shouldBe` empty

    it "Should read single char" $ do
      fromLines "c" `shouldBe` fromList [['c']]

    it "Should read small grid" $ do
      fromLines "ab\ncd\nef" `shouldBe` fromList ["ab", "cd", "ef"]

    it "Should read medium grid" $ do
      fromLines "abc\ndef\nghi" `shouldBe` fromList ["abc", "def", "ghi"]

    it "Should read larger grid" $ do
      fromLines "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD"
        `shouldBe` fromList
          [ "abcdefgh",
            "ijklmnop",
            "qrstuvwx",
            "yz012345",
            "6789ABCD"
          ]

  describe "When exporting Grid toList" $ do
    it "Should create empty list from empty Grid" $ do
      toList empty `shouldBe` []

    it "Should create single char list from single char Grid" $ do
      toList (fromList [['c']]) `shouldBe` [['c']]

    it "Should create larger char list from larger char Grid" $ do
      toList (fromLines "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD")
        `shouldBe` [ "abcdefgh",
                     "ijklmnop",
                     "qrstuvwx",
                     "yz012345",
                     "6789ABCD"
                   ]

  describe "When accessing by coordinates" $ do
    let largerGrid =
          fromList
            [ "abcdefgh",
              "ijklmnop",
              "qrstuvwx",
              "yz012345",
              "6789ABCD"
            ]

    it "Should access (0, 0)" $ do
      lookup (0, 0) largerGrid `shouldBe` Just 'a'

    it "Should access (1, 0)" $ do
      lookup (1, 0) largerGrid `shouldBe` Just 'b'

    it "Should access (0, 1)" $ do
      lookup (0, 1) largerGrid `shouldBe` Just 'i'

    it "Should access (2, 0)" $ do
      lookup (2, 0) largerGrid `shouldBe` Just 'c'

    it "Should access (0, 2)" $ do
      lookup (0, 2) largerGrid `shouldBe` Just 'q'

    it "Should access (2, 1)" $ do
      lookup (2, 1) largerGrid `shouldBe` Just 'k'

    it "Should access (1, 2)" $ do
      lookup (1, 2) largerGrid `shouldBe` Just 'r'

    it "Should NOT find anything at (-1, -1)" $ do
      lookup (-1, -1) largerGrid `shouldBe` Nothing

    it "Should NOT find anything at (-1, -2)" $ do
      lookup (-1, -2) largerGrid `shouldBe` Nothing

    it "Should NOT find anything at (8, 0)" $ do
      lookup (8, 0) largerGrid `shouldBe` Nothing

    it "Should NOT find anything at (0, 8)" $ do
      lookup (0, 8) largerGrid `shouldBe` Nothing

  describe "When access columns and rows" $ do
    let largerGrid =
          fromList
            [ "abcdefgh",
              "ijklmnop",
              "qrstuvwx",
              "yz012345",
              "6789ABCD"
            ]

    it "Should provide all columns" $ do
      columns largerGrid
        `shouldBe` [ [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)],
                     [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4)],
                     [(2, 0), (2, 1), (2, 2), (2, 3), (2, 4)],
                     [(3, 0), (3, 1), (3, 2), (3, 3), (3, 4)],
                     [(4, 0), (4, 1), (4, 2), (4, 3), (4, 4)],
                     [(5, 0), (5, 1), (5, 2), (5, 3), (5, 4)],
                     [(6, 0), (6, 1), (6, 2), (6, 3), (6, 4)],
                     [(7, 0), (7, 1), (7, 2), (7, 3), (7, 4)]
                   ]

    it "Should provide all rows" $ do
      rows largerGrid
        `shouldBe` [ [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
                     [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1)],
                     [(0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (7, 2)],
                     [(0, 3), (1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3)],
                     [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4), (5, 4), (6, 4), (7, 4)]
                   ]

    it "Should provide subgrid" $ do
      (toList . subgrid (2, 2) (2, 3) $ largerGrid)
        `shouldBe` [ "st",
                     "01",
                     "89"
                   ]

  describe "When accessing neigbors" $ do
    let largerGrid =
          fromList
            [ "abcdefgh",
              "ijklmnop",
              "qrstuvwx",
              "yz012345",
              "6789ABCD"
            ]

    it "Should get allNorthOf (0, 0)" $ do
      allNorthOf (0, 0) largerGrid `shouldBe` []

    it "Should get allSouthOf (0, 0)" $ do
      allSouthOf (0, 0) largerGrid `shouldBe` [(0, 1), (0, 2), (0, 3), (0, 4)]

    it "Should get allWestOf (0, 0)" $ do
      allWestOf (0, 0) largerGrid `shouldBe` []

    it "Should get allEastOf (0, 0)" $ do
      allEastOf (0, 0) largerGrid `shouldBe` [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)]

    it "Should get allNorthOf (2, 3)" $ do
      allNorthOf (2, 3) largerGrid `shouldBe` [(2, 2), (2, 1), (2, 0)]

    it "Should get allSouthOf (2, 3)" $ do
      allSouthOf (2, 3) largerGrid `shouldBe` [(2, 4)]

    it "Should get allWestOf (2, 3)" $ do
      allWestOf (2, 3) largerGrid `shouldBe` [(1, 3), (0, 3)]

    it "Should get allEastOf (2, 3)" $ do
      allEastOf (2, 3) largerGrid `shouldBe` [(3, 3), (4, 3), (5, 3), (6, 3), (7, 3)]

    it "Should get allNorthOf (3, 4)" $ do
      allNorthOf (3, 4) largerGrid `shouldBe` [(3, 3), (3, 2), (3, 1), (3, 0)]

    it "Should get allSouthOf (3, 4)" $ do
      allSouthOf (3, 4) largerGrid `shouldBe` []

    it "Should get allWestOf (3, 4)" $ do
      allWestOf (3, 4) largerGrid `shouldBe` [(2, 4), (1, 4), (0, 4)]

    it "Should get allEastOf (3, 4)" $ do
      allEastOf (3, 4) largerGrid `shouldBe` [(4, 4), (5, 4), (6, 4), (7, 4)]

  describe "When inserting item" $ do
    let one = insert (3, 4) 'a' empty

    it "Should have width 1" $ do
      width one `shouldBe` 1

    it "Should have height 1" $ do
      height one `shouldBe` 1

    it "Should export toList" $ do
      toList one `shouldBe` [['a']]

    describe "When inserting second item" $ do
      let two = insert (5, 2) 'b' one

      it "Should have width 3" $ do
        width two `shouldBe` 3

      it "Should have height 3" $ do
        height two `shouldBe` 3

      it "Should extend Grid" $ do
        toList two
          `shouldBe` [ "__b",
                       "___",
                       "a__"
                     ]
