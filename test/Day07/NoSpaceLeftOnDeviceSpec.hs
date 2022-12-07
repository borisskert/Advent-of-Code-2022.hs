module Day07.NoSpaceLeftOnDeviceSpec (spec) where

import Day07.NoSpaceLeftOnDevice
import Test.Hspec

exampleInput :: String
exampleInput = "- / (dir)\n  - a (dir)\n    - e (dir)\n      - i (file, size=584)\n    - f (file, size=29116)\n    - g (file, size=2557)\n    - h.lst (file, size=62596)\n  - b.txt (file, size=14848514)\n  - c.dat (file, size=8504156)\n  - d (dir)\n    - j (file, size=4060174)\n    - d.log (file, size=8033020)\n    - d.ext (file, size=5626152)\n    - k (file, size=7214296)\n"

spec :: Spec
spec = do
  describe "What is the sum of the total sizes of those directories?" $ do
    it "In the example above, these directories are a and e; the sum of their total sizes is 95437 (94853 + 584)" $ do
      totalSize exampleInput `shouldBe` 95437
