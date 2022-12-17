module Day11.Item (Item, from) where

newtype Item = Item Int deriving (Show, Eq)

from :: Int -> Item
from = Item
