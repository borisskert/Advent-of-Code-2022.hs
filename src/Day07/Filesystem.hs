module Day07.Filesystem (Filesystem, fromList, empty, touch, root, size, folders) where

import Common.Tree (Tree, elems, insert, subtrees)
import qualified Common.Tree as Tree (empty, fromList)

type Directory = Tree String Integer

newtype Filesystem = Filesystem Directory deriving (Eq, Show)

type Path = [String]

empty :: Filesystem
empty = Filesystem Tree.empty

touch :: Path -> Integer -> Filesystem -> Filesystem
touch path fileSize (Filesystem tree) = Filesystem . insert path fileSize $ tree

fromList :: [(Path, Integer)] -> Filesystem
fromList = Filesystem . Tree.fromList

folders :: Directory -> [Directory]
folders = tail . subtrees

root :: Filesystem -> Directory
root (Filesystem rootDir) = rootDir

size :: Directory -> Integer
size = sum . elems
