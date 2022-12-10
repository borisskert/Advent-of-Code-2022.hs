module Day07.NoSpaceLeftOnDeviceSpec (spec) where

import Day07.NoSpaceLeftOnDevice
import Test.Hspec

exampleInput :: String
exampleInput = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

spec :: Spec
spec = do
  describe "What is the sum of the total sizes of those directories?" $ do
    it "In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584)" $ do
      totalSize exampleInput `shouldBe` 95437

  describe "Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. What is the total size of that directory?" $ do
    it "However, directories d and / are both big enough! Between these, choose the smallest: d, increasing unused space by 24933642." $ do
      totalSizeOfDirectoryToDelete exampleInput `shouldBe` 24933642
  