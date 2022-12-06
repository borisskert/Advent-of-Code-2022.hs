module Day05.Crate (Crate, crateOf, name, readLine) where

newtype Crate = Crate Char deriving (Eq, Show)

crateOf :: Char -> Crate
crateOf = Crate

name :: Crate -> Char
name (Crate c) = c

readLine :: String -> [Maybe Crate]
readLine [] = []
readLine xs = (crate :) . readLine . drop 4 $ xs
  where
    crateName = head . tail $ xs
    crate
      | crateName == ' ' = Nothing
      | otherwise = Just . crateOf $ crateName
