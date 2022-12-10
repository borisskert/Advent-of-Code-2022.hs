module Day07.BashSpec (spec) where

import Day07.Bash
import Day07.Command (readMany)
import Day07.Filesystem (fromList)
import Test.Hspec

exampleInput :: String
exampleInput = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

spec :: Spec
spec = do
  describe "Should reverse engineer Bash history" $ do
    it "Should reverse-engineer example" $ do
      (operate . readMany . lines $ exampleInput)
        `shouldBe` fromList
          [ (["a", "e", "i"], 584),
            (["a", "f"], 29116),
            (["a", "g"], 2557),
            (["a", "h.lst"], 62596),
            (["b.txt"], 14848514),
            (["c.dat"], 8504156),
            (["d", "j"], 4060174),
            (["d", "d.log"], 8033020),
            (["d", "d.ext"], 5626152),
            (["d", "k"], 7214296)
          ]
