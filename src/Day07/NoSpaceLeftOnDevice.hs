{-# LANGUAGE NumericUnderscores #-}

module Day07.NoSpaceLeftOnDevice (totalSize, totalSizeOfDirectoryToDelete) where

import Day07.Filesystem
import Day07.Command
import Day07.Bash

totalSize :: String -> Integer
totalSize = sum . filter (<= 100_000) . map size . folders . root . readFilesystem

totalSizeOfDirectoryToDelete :: String -> Integer
totalSizeOfDirectoryToDelete input = minimum
 . filter (>= neededSpace) . map size . folders $ rootDir
  where
    rootDir = root . readFilesystem $ input
    diskSize = 70_000_000 :: Integer
    consumedSpace = size rootDir :: Integer
    freeSpace = diskSize - consumedSpace :: Integer
    neededSpace = 30_000_000 - freeSpace:: Integer

readFilesystem :: String -> Filesystem
readFilesystem input = operate . readMany . lines $ input:: Filesystem
