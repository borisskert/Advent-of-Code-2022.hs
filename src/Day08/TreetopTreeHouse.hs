module Day08.TreetopTreeHouse (howManyTrees) where

import Day08.Forrest

howManyTrees :: String -> Int
howManyTrees = length . visibleTrees . readFrom
