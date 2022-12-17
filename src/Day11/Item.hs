module Day11.Item (Item, from, worryLevel) where

type WorryLevel = Int
  
newtype Item = Item WorryLevel deriving (Show, Eq)

from :: WorryLevel -> Item
from = Item

worryLevel :: Item -> WorryLevel
worryLevel (Item level) = level
