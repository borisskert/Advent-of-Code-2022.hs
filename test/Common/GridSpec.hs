module Common.GridSpec (spec) where

import Common.Grid (Grid, height, width)
import Common.GridSpec.TestGrid
import Common.OctaGridPosition (Position)
import Test.Hspec
import Prelude hiding (lookup)

largerGrid :: Grid Position TestValue
largerGrid =
  fromList
    [ ((0, 0), 'a'),
      ((0, 1), 'i'),
      ((0, 2), 'q'),
      ((0, 3), 'y'),
      ((0, 4), '6'),
      ((1, 0), 'b'),
      ((1, 1), 'j'),
      ((1, 2), 'r'),
      ((1, 3), 'z'),
      ((1, 4), '7'),
      ((2, 0), 'c'),
      ((2, 1), 'k'),
      ((2, 2), 's'),
      ((2, 3), '0'),
      ((2, 4), '8'),
      ((3, 0), 'd'),
      ((3, 1), 'l'),
      ((3, 2), 't'),
      ((3, 3), '1'),
      ((3, 4), '9'),
      ((4, 0), 'e'),
      ((4, 1), 'm'),
      ((4, 2), 'u'),
      ((4, 3), '2'),
      ((4, 4), 'A'),
      ((5, 0), 'f'),
      ((5, 1), 'n'),
      ((5, 2), 'v'),
      ((5, 3), '3'),
      ((5, 4), 'B'),
      ((6, 0), 'g'),
      ((6, 1), 'o'),
      ((6, 2), 'w'),
      ((6, 3), '4'),
      ((6, 4), 'C'),
      ((7, 0), 'h'),
      ((7, 1), 'p'),
      ((7, 2), 'x'),
      ((7, 3), '5'),
      ((7, 4), 'D')
    ]

spec :: Spec
spec = do
  describe "When read Grid from lines" $ do
    it "Should read empty Grid" $ do
      read "" `shouldBe` empty

    it "Should read single char" $ do
      read "c" `shouldBe` fromList [((0, 0), 'c')]

    it "Should read small grid" $ do
      read "ab\ncd\nef"
        `shouldBe` fromList
          [ ((0, 0), 'a'),
            ((1, 0), 'b'),
            ((0, 1), 'c'),
            ((1, 1), 'd'),
            ((0, 2), 'e'),
            ((1, 2), 'f')
          ]

    it "Should read medium grid" $ do
      read "abc\ndef\nghi"
        `shouldBe` fromList
          [ ((0, 0), 'a'),
            ((1, 0), 'b'),
            ((2, 0), 'c'),
            ((0, 1), 'd'),
            ((1, 1), 'e'),
            ((2, 1), 'f'),
            ((0, 2), 'g'),
            ((1, 2), 'h'),
            ((2, 2), 'i')
          ]

    it "Should read larger grid" $ do
      read "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD"
        `shouldBe` largerGrid

  describe "When exporting Grid toList" $ do
    it "Should create empty list from empty Grid" $ do
      toList empty `shouldBe` []

    it "Should create single char list from single char Grid" $ do
      toList (fromList [((0, 0), 'c')]) `shouldBe` [((0, 0), 'c')]

    it "Should create larger char list from larger char Grid" $ do
      toList (read "abcdefgh\nijklmnop\nqrstuvwx\nyz012345\n6789ABCD")
        `shouldBe` [((0, 0), 'a'), ((0, 1), 'i'), ((0, 2), 'q'), ((0, 3), 'y'), ((0, 4), '6'), ((1, 0), 'b'), ((1, 1), 'j'), ((1, 2), 'r'), ((1, 3), 'z'), ((1, 4), '7'), ((2, 0), 'c'), ((2, 1), 'k'), ((2, 2), 's'), ((2, 3), '0'), ((2, 4), '8'), ((3, 0), 'd'), ((3, 1), 'l'), ((3, 2), 't'), ((3, 3), '1'), ((3, 4), '9'), ((4, 0), 'e'), ((4, 1), 'm'), ((4, 2), 'u'), ((4, 3), '2'), ((4, 4), 'A'), ((5, 0), 'f'), ((5, 1), 'n'), ((5, 2), 'v'), ((5, 3), '3'), ((5, 4), 'B'), ((6, 0), 'g'), ((6, 1), 'o'), ((6, 2), 'w'), ((6, 3), '4'), ((6, 4), 'C'), ((7, 0), 'h'), ((7, 1), 'p'), ((7, 2), 'x'), ((7, 3), '5'), ((7, 4), 'D')]

  describe "When accessing by coordinates" $ do
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
        `shouldBe` [((2, 2), 's'), ((2, 3), '0'), ((2, 4), '8'), ((3, 2), 't'), ((3, 3), '1'), ((3, 4), '9')]

  describe "When accessing neigbors" $ do
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
      toList one `shouldBe` [((3, 4), 'a')]

    describe "When inserting second item" $ do
      let two = insert (5, 2) 'b' one

      it "Should have width 3" $ do
        width two `shouldBe` 3

      it "Should have height 3" $ do
        height two `shouldBe` 3

      it "Should extend Grid" $ do
        show two `shouldBe` "__b\n___\na__"
