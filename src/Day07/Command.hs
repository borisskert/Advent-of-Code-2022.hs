{-# LANGUAGE QuasiQuotes #-}

module Day07.Command (readMany, cd, ls, dir, file) where

import Common.Regex
import Data.List (isPrefixOf)

data LsEntry = Directory String | File Integer String deriving (Eq, Show)

data Command = ChangeDir String | ListDir [LsEntry] deriving (Eq, Show)

readMany :: [String] -> [Command]
readMany [] = []
readMany xs = one : (readMany . drop (entries one) $ xs)
  where
    one = readOne xs

isChangeDir :: String -> Bool
isChangeDir = isMatch changeDirPattern

changeDirPattern :: Regex
changeDirPattern = [re|\$ cd (.+)|]

readOne :: [String] -> Command
readOne s
  | isChangeDir . head $ s = ChangeDir changeDirName
  | otherwise = ListDir lsEntries
  where
    changeDirName = head . parseGroups changeDirPattern . head $ s
    lsEntries = map readLsEntry . takeWhile (not . ("$ " `isPrefixOf`)) . tail $ s

cd :: String -> Command
cd = ChangeDir

ls :: [LsEntry] -> Command
ls = ListDir

dir :: String -> LsEntry
dir = Directory

file :: Integer -> String -> LsEntry
file = File

entries :: Command -> Int
entries (ChangeDir _) = 1
entries (ListDir xs) = length xs + 1

readLsEntry :: String -> LsEntry
readLsEntry s
  | isMatch filePattern s = File fileSize filename
  | otherwise = Directory directoryName
  where
    filePattern = [re|([0-9]+) (.+)|]
    fileAttributes = parseGroups filePattern s
    fileSize = read . head $ fileAttributes
    filename = last fileAttributes

    directoryPattern = [re|dir (.+)|]
    directoryName = head . parseGroups directoryPattern $ s
