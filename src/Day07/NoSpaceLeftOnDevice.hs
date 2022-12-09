{-# LANGUAGE NumericUnderscores #-}

module Day07.NoSpaceLeftOnDevice (totalSize) where

import Day07.Filesystem
import Day07.Command
import Day07.Bash

totalSize :: String -> Integer
totalSize input = sum . filter (<= 100_000) . map size . folders . root $ fs
  where
    fs = operate . readMany . lines $ input:: Filesystem
