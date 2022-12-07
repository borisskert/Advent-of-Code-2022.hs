{-# LANGUAGE QuasiQuotes #-}

module Day07.Filesystem (Entry, readFrom, dir, file, size) where

import Common.List
import Common.Regex
import Data.Maybe (fromMaybe)

data Entry = Directory {dirname :: String, children :: [Entry], dirsize :: Integer} | File {filename :: String, filesize :: Integer} deriving (Eq, Show)

readFrom :: String -> [Entry]
readFrom = readFromLines . lines

readFromLines :: [String] -> [Entry]
readFromLines [] = []
readFromLines xs
  | isDir . head $ xs = directory : readFromLines furtherLines
  | isFile . head $ xs = file' : readFromLines (tail xs)
  | otherwise = error ("Stop at: " ++ head xs)
  where
    directory = readDir xs
    file' = readFileEntry . head $ xs
    furtherLines = drop (entries directory) xs

isDir :: String -> Bool
isDir = isMatch [re|- (.+) \(dir\)|]

isFile :: String -> Bool
isFile = isMatch [re|- (.+) \(file, size=[0-9]+\)|]

isSubEntry :: String -> Bool
isSubEntry = isMatch [re|(  )+- (.+) \(.*\)|]

parseDirName :: String -> String
parseDirName = head . parseGroups [re|- (.+) \(dir\)|]

readDir :: [String] -> Entry
readDir input = Directory {dirname = name, children = dirChildren, dirsize = childrenSize}
  where
    name = parseDirName . head $ input
    dirChildren = readFromLines . map (drop 2) . takeWhile isSubEntry . fromMaybe [] . safeTail $ input
    childrenSize = sum . map size $ dirChildren

readFileEntry :: String -> Entry
readFileEntry input = File {filename = myName, filesize = mySize}
  where
    attributes = parseGroups [re|- (.+) \(file, size=([0-9]+)\)|] input :: [String]
    myName = head attributes
    mySize = read . last $ attributes :: Integer

dir :: String -> [Entry] -> Entry
dir myName myEntries = Directory {dirname = myName, children = myEntries, dirsize = mySize}
  where
    mySize = sum . map size $ myEntries

file :: String -> Integer -> Entry
file myName mySize = File {filename = myName, filesize = mySize}

size :: Entry -> Integer
size Directory {dirname = _, children = _, dirsize = mySize} = mySize
size File {filename = _, filesize = mySize} = mySize

entries :: Entry -> Int
entries Directory {dirname = _, children = myChildren, dirsize = _} = (+ 1) . sum . map entries $ myChildren
entries _ = 1
