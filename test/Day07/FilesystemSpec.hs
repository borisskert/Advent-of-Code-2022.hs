module Day07.FilesystemSpec (spec) where

import Day07.Filesystem
import Test.Hspec

exampleInput :: String
exampleInput = "- / (dir)\n  - a (dir)\n    - e (dir)\n      - i (file, size=584)\n    - f (file, size=29116)\n    - g (file, size=2557)\n    - h.lst (file, size=62596)\n  - b.txt (file, size=14848514)\n  - c.dat (file, size=8504156)\n  - d (dir)\n    - j (file, size=4060174)\n    - d.log (file, size=8033020)\n    - d.ext (file, size=5626152)\n    - k (file, size=7214296)\n"

spec :: Spec
spec = do
  describe "Create Filesystem Entries" $ do
    it "Should read from example input" $ do
      readFrom exampleInput
        `shouldBe` [ dir
                       "/"
                       [ dir
                           "a"
                           [ dir
                               "e"
                               [file "i" 584],
                             file "f" 29116,
                             file "g" 2557,
                             file "h.lst" 62596
                           ],
                         file "b.txt" 14848514,
                         file "c.dat" 8504156,
                         dir
                           "d"
                           [ file "j" 4060174,
                             file "d.log" 8033020,
                             file "d.ext" 5626152,
                             file "k" 7214296
                           ]
                       ]
                   ]
