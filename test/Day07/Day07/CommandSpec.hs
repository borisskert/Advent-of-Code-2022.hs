module Day07.Day07.CommandSpec where

import Day07.Command
import Test.Hspec

exampleInput :: String
exampleInput = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

spec :: Spec
spec = do
  describe "When parsing Commands" $ do
    it "Should read example" $ do
      (readMany . lines $ exampleInput)
        `shouldBe` [ cd "/",
                     ls
                       [ dir "a",
                         file 14848514 "b.txt",
                         file 8504156 "c.dat",
                         dir "d"
                       ],
                     cd "a",
                     ls
                       [ dir "e",
                         file 29116 "f",
                         file 2557 "g",
                         file 62596 "h.lst"
                       ],
                     cd "e",
                     ls
                       [ file 584 "i"
                       ],
                     cd "..",
                     cd "..",
                     cd "d",
                     ls
                       [ file 4060174 "j",
                         file 8033020 "d.log",
                         file 5626152 "d.ext",
                         file 7214296 "k"
                       ]
                   ]
