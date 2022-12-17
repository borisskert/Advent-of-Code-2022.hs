{-# LANGUAGE QuasiQuotes #-}

module Day11.Items (Items, fromList) where

import Common.Regex
import Data.List.Split (splitOn)
import Day11.Item

newtype Items = Items [Item] deriving (Show, Eq)

fromList :: [Item] -> Items
fromList = Items

linePattern :: Regex
linePattern = [re|[ ]*Starting items: ([0-9, ]+)|]

instance Read Items where
  readsPrec _ input = [(parse input, [])]
    where
      parse = Items . map (from . read) . splitOn ", " . head . parseGroups linePattern
