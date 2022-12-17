module Day11.Item (Item, from, worryLevel, normalize) where

import Day11.WorryLevel (WorryLevel)

newtype Item = Item WorryLevel deriving (Show, Eq)

from :: WorryLevel -> Item
from = Item

worryLevel :: Item -> WorryLevel
worryLevel (Item level) = level

normalize :: WorryLevel -> Item -> Item
normalize worry (Item item) = Item (item `mod` worry)
