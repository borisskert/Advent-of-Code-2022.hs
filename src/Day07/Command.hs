{-# LANGUAGE QuasiQuotes #-}

module Day07.Command
  ( Command,
    LsEntry,
    readMany,
    cd,
    ls,
    dir,
    file,
    isCd,
    dirname,
    lsEntries,
    isDir,
    entryName,
    entrySize,
  )
where

import Common.Regex
import Data.List (isPrefixOf)

data LsEntry = Directory String | File Integer String deriving (Eq, Show)

data Command = ChangeDir String | ListDir [LsEntry] deriving (Eq, Show)

readMany :: [String] -> [Command]
readMany [] = []
readMany [""] = [] -- TODO
readMany xs = one : (readMany . drop (entries one) $ xs)
  where
    one = readOne xs

isChangeDir :: String -> Bool
isChangeDir = isMatch changeDirPattern

changeDirPattern :: Regex
changeDirPattern = [re|\$ cd (.+)|]

readOne :: [String] -> Command
readOne [] = error "Command.readOne: input empty"
readOne s
  | isChangeDir . head $ s = ChangeDir changeDirName
  | otherwise = ListDir lsCmdEntries
  where
    parsed = parseGroups changeDirPattern . head $ s
    changeDirName
      | null parsed = error "nothing parsed"
      | otherwise = head . parseGroups changeDirPattern . head $ s
    lsCmdEntries = map readLsEntry . takeWhile (not . ("$ " `isPrefixOf`)) . tail $ s

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
    fileSize
      | not . null $ fileAttributes = read . head $ fileAttributes
      | otherwise = error "File attributes null"
    filename = last fileAttributes

    directoryPattern = [re|dir (.+)|]
    parsed2 = parseGroups directoryPattern s
    directoryName
      | null parsed2 = error "Nothing parsed 2"
      | otherwise = head . parseGroups directoryPattern $ s

isCd :: Command -> Bool
isCd (ChangeDir _) = True
isCd _ = False

dirname :: Command -> String
dirname (ChangeDir name) = name
dirname _ = error "`ls` has no dirname"

lsEntries :: Command -> [LsEntry]
lsEntries (ListDir xs) = xs
lsEntries _ = error "`cd` has no entries"

isDir :: LsEntry -> Bool
isDir (Directory _) = True
isDir _ = False

entryName :: LsEntry -> String
entryName (Directory name) = name
entryName (File _ name) = name

entrySize :: LsEntry -> Integer
entrySize (File size _) = size
entrySize _ = error "Day07.Command.entrySize: Not a File"
