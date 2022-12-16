module Day11.Item (Item, item) where

newtype Item = Item Int deriving (Show, Eq)

item :: Int -> Item
item = Item
