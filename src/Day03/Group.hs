module Day03.Group (Group (Group), readMany, badge) where

import Data.List.Split (chunksOf)
import Day03.Item (Item)
import Day03.Rucksack (Rucksack, contains, items, readOne)

data Group = Group Rucksack Rucksack Rucksack deriving (Show, Eq)

from :: [String] -> Group
from [a, b, c] = Group (readOne a) (readOne b) (readOne c)
from _ = error "Illegal input"

readMany :: String -> [Group]
readMany = map from . chunksOf 3 . lines

badge :: Group -> Item
badge (Group a b c) = head . filter (\i -> b `contains` i && c `contains` i) . items $ a
