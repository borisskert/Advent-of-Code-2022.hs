module Day11.Item (Item, from, worryLevel, normalize) where

type WorryLevel = Integer

newtype Item = Item WorryLevel deriving (Show, Eq)

from :: WorryLevel -> Item
from = Item

worryLevel :: Item -> WorryLevel
worryLevel (Item level) = level

normalize :: WorryLevel -> Item -> Item
normalize worry (Item item) = Item (item `mod` worry)
--
--denormalize :: WorryLevel -> Item -> Item
--denormalize worry (Item item) = Item (item * worry)
