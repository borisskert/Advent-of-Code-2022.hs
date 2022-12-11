module Common.CrossGridSpec (spec) where

import Common.CrossGrid
import Test.Hspec
import Prelude hiding (lookup)

spec :: Spec
spec = do
  describe "When read CrossGrid from lines" $ do
    it "Should read empty CrossGrid" $ do
      fromLines (\(_, c) -> Just c) "" `shouldBe` empty

    it "Should read single char" $ do
      fromLines (\(_, c) -> Just c) "c" `shouldBe` fromList [['c']]

    it "Should read small grid" $ do
      fromLines (\(_, c) -> Just c) "ab\ncd\nef" `shouldBe` fromList ["ab", "cd", "ef"]

    it "Should read medium grid" $ do
      fromLines (\(_, c) -> Just c) "abc\ndef\nghi" `shouldBe` fromList ["abc", "def", "ghi"]

    it "Should read larger grid" $ do
      fromLines (\(_, c) -> Just c) "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD"
        `shouldBe` fromList
          [ "abcdefgh",
            "ijklmnop",
            "qrstuvwx",
            "yz012345",
            "6789ABCD"
          ]

  describe "When exporting CrossGrid toList" $ do
    it "Should create empty list from empty CrossGrid" $ do
      toList '_' (empty :: CrossGrid Char) `shouldBe` []

    it "Should create single char list from single char CrossGrid" $ do
      toList '_' (fromList [['c']]) `shouldBe` [['c']]

    it "Should create larger char list from larger char CrossGrid" $ do
      toList '_' (fromLines (\(_, c) -> Just c) "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD")
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
