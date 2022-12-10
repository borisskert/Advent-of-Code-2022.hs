{-# LANGUAGE QuasiQuotes #-}

module Day07.Command
  ( Command,
    LsFile,
    readMany,
    cd,
    ls,
    dir,
    file,
    isCd,
    dirname,
    lsFiles,
    fileName,
    fileSize,
  )
where

import Common.Regex
import Data.List (isPrefixOf)

data LsFile = Directory String | File Integer String deriving (Eq, Show)

data Command = ChangeDir String | ListDir [LsFile] deriving (Eq, Show)

readMany :: [String] -> [Command]
readMany [] = []
readMany xs = one : (readMany . drop (count one) $ xs)
  where
    one = readOne xs

isChangeDir :: String -> Bool
isChangeDir = isMatch changeDirPattern

changeDirPattern :: Regex
changeDirPattern = [re|\$ cd (.+)|]

readOne :: [String] -> Command
readOne s
  | isChangeDir . head $ s = ChangeDir changeDirName
  | otherwise = ListDir lsCmdEntries
  where
    changeDirName = head . parseGroups changeDirPattern . head $ s
    lsCmdEntries = map readLsEntry . takeWhile (not . ("$ " `isPrefixOf`)) . tail $ s

cd :: String -> Command
cd = ChangeDir

ls :: [LsFile] -> Command
ls = ListDir

dir :: String -> LsFile
dir = Directory

file :: Integer -> String -> LsFile
file = File

count :: Command -> Int
count (ChangeDir _) = 1
count (ListDir xs) = length xs + 1

readLsEntry :: String -> LsFile
readLsEntry s
  | isMatch filePattern s = File size filename
  | otherwise = Directory directoryName
  where
    filePattern = [re|([0-9]+) (.+)|]
    fileAttributes = parseGroups filePattern s
    size = read . head $ fileAttributes
    filename = last fileAttributes

    directoryPattern = [re|dir (.+)|]
    directoryName = head . parseGroups directoryPattern $ s

isCd :: Command -> Bool
isCd (ChangeDir _) = True
isCd _ = False

dirname :: Command -> String
dirname (ChangeDir name) = name
dirname _ = error "`ls` has no dirname"

lsFiles :: Command -> [LsFile]
lsFiles (ListDir xs) = xs
lsFiles _ = error "`cd` has no entries"

fileName :: LsFile -> String
fileName (Directory name) = name
fileName (File _ name) = name

fileSize :: LsFile -> Integer
fileSize (File size _) = size
fileSize _ = error "Day07.Command.entrySize: Not a File"
