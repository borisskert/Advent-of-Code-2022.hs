module Day10.Instruction (Instruction, execute, cycles) where

import Data.List.Split (splitOn)

data Instruction = Noop | AddX Int deriving (Eq, Show)

instance Read Instruction where
  readsPrec _ "noop" = [(Noop, [])]
  readsPrec _ input = [(readAddX input, [])]

readAddX :: String -> Instruction
readAddX = AddX . read . last . splitOn " "

execute :: Int -> Instruction -> Int
execute register Noop = register
execute register (AddX value) = register + value

cycles :: Instruction -> Int
cycles Noop = 1
cycles (AddX _) = 2
