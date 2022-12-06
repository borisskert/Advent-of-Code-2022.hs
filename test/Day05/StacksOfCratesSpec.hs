module Day05.StacksOfCratesSpec (spec) where

import Day05.Crate
import Day05.Move
import Day05.StacksOfCrates
import Test.Hspec

exampleInput :: String
exampleInput = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 "

exampleStacks :: StacksOfCrates
exampleStacks =
  fromList
    [ ('1', [crateOf 'N', crateOf 'Z']),
      ('2', [crateOf 'D', crateOf 'C', crateOf 'M']),
      ('3', [crateOf 'P'])
    ]

spec :: Spec
spec = do
  describe "When reading input" $ do
    it "Should read example" $ do
      (readFrom . lines $ exampleInput) `shouldBe` exampleStacks

  describe "When perform moves" $ do
    it "Should perform first example move" $ do
      move (moveOf 1 '2' '1') exampleStacks
        `shouldBe` fromList
          [ ('1', [crateOf 'D', crateOf 'N', crateOf 'Z']),
            ('2', [crateOf 'C', crateOf 'M']),
            ('3', [crateOf 'P'])
          ]

    it "Should perform first two example moves" $ do
      (move (moveOf 3 '1' '3') . move (moveOf 1 '2' '1') $ exampleStacks)
        `shouldBe` fromList
          [ ('1', []),
            ('2', [crateOf 'C', crateOf 'M']),
            ('3', [crateOf 'Z', crateOf 'N', crateOf 'D', crateOf 'P'])
          ]

  describe "When perform moves by CrateMover 9001" $ do
    it "Should perform first example move" $ do
      moveN (moveOf 1 '2' '1') exampleStacks
        `shouldBe` fromList
          [ ('1', [crateOf 'D', crateOf 'N', crateOf 'Z']),
            ('2', [crateOf 'C', crateOf 'M']),
            ('3', [crateOf 'P'])
          ]

    it "Should perform first two example moves" $ do
      (moveN (moveOf 3 '1' '3') . moveN (moveOf 1 '2' '1') $ exampleStacks)
        `shouldBe` fromList
          [ ('1', []),
            ('2', [crateOf 'C', crateOf 'M']),
            ('3', [crateOf 'D', crateOf 'N', crateOf 'Z', crateOf 'P'])
          ]
