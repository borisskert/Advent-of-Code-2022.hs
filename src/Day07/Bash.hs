module Day07.Bash (operate) where

import Day07.Command (Command, LsFile, dirname, fileName, fileSize, isCd, lsFiles)
import Day07.Filesystem (Filesystem, empty, touch)

type WorkingDir = [String]

data Bash = Bash WorkingDir Filesystem

operate :: [Command] -> Filesystem
operate cs = fs
  where
    (Bash _ fs) = foldl (flip operateOn) (Bash ["/"] empty) cs

operateOn :: Command -> Bash -> Bash
operateOn command bash
  | isCd command = cd command bash
  | otherwise = ls command bash

cd :: Command -> Bash -> Bash
cd changeDir (Bash cwd fs)
  | name == "/" = Bash [] fs
  | name == "." = Bash cwd fs
  | name == ".." = Bash (init cwd) fs
  | otherwise = Bash (cwd ++ [name]) fs
  where
    name = dirname changeDir

ls :: Command -> Bash -> Bash
ls command bash = foldl (flip touchFiles) bash files
  where
    files = lsFiles command

touchFiles :: LsFile -> Bash -> Bash
touchFiles lsEntry (Bash cwd fs) = Bash cwd touchedFile
  where
    name = fileName lsEntry
    size = fileSize lsEntry
    touchedFile = touch (cwd ++ [name]) size fs
