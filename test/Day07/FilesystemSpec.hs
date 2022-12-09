module Day07.FilesystemSpec (spec) where

import Test.Hspec
import Day07.Filesystem

simpleFilesystem :: Filesystem
simpleFilesystem = fromList [(["b.txt"], 14848514)]

stillSimpleFilesystem :: Filesystem
stillSimpleFilesystem = fromList [(["b.txt"], 14848514), (["c.dat"], 8504156)]

filesystemWithSubfolder :: Filesystem
filesystemWithSubfolder = fromList [(["a", "e", "i"], 584)]


exampleFilesystem :: Filesystem
exampleFilesystem = fromList [ (["a", "e", "i"], 584),
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

spec :: Spec
spec = do
  describe "When using Filesystems" $ do
    it "Should determine size of empty Filesystem" $ do
      (size . root $ empty) `shouldBe` 0

    it "Should determine size of simple Filesystem" $ do
      (size . root $ simpleFilesystem) `shouldBe` 14848514

    it "Should determine size of still simple Filesystem" $ do
      (size . root $ stillSimpleFilesystem) `shouldBe` 14848514 + 8504156

    it "Should determine size of Filesystem with subfolder" $ do
      (size . root $ filesystemWithSubfolder) `shouldBe` 584

    it "Should determine size of example Filesystem" $ do
      (size . root $ exampleFilesystem) `shouldBe` 48381165
