module Day08.TreetopTreeHouse (howManyTrees, highestScenicScore) where

import Day08.Forrest

howManyTrees :: String -> Int
howManyTrees = length . visibleTrees . readFrom

highestScenicScore :: String -> Int
highestScenicScore s = maximum . map (`scenicScore` forrest) . allTrees $ forrest
  where
    forrest = readFrom s
