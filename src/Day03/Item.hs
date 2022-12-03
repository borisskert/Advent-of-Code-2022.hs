module Day03.Item (Item, from, fromMany, priority) where

import Data.Char (isLower, isUpper, ord)

newtype Item = Item Char deriving (Show, Eq)

from :: Char -> Item
from = Item

fromMany :: String -> [Item]
fromMany = map from

priority :: Item -> Int
priority (Item c)
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = error ("Illegal item '" ++ [c] ++ "'")
