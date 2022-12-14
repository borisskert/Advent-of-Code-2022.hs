{-# LANGUAGE QuasiQuotes #-}

module Day11.Items (Items, fromList, toList, current, remaining, append) where

import Common.List (safeHead)
import qualified Common.List as List (append)
import Common.Regex
import Data.List.Split (splitOn)
import Day11.Item

newtype Items = Items [Item] deriving (Show, Eq)

fromList :: [Item] -> Items
fromList = Items

toList :: Items -> [Item]
toList (Items xs) = xs

linePattern :: Regex
linePattern = [re|[ ]*Starting items: ([0-9, ]+)|]

instance Read Items where
  readsPrec _ input = [(parse input, [])]
    where
      parse = Items . map (from . read) . splitOn ", " . head . parseGroups linePattern

current :: Items -> Maybe Item
current (Items xs) = safeHead xs

remaining :: Items -> Items
remaining items@(Items []) = items
remaining (Items xs) = Items . tail $ xs

append :: Item -> Items -> Items
append item (Items items) = Items . List.append item $ items
