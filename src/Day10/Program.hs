module Day10.Program (Program, empty, current, remaining, isFinished) where

import Data.List (intercalate)
import Day10.Instruction

newtype Program = Program [Instruction]

empty :: Program
empty = Program []

instance Read Program where
  readsPrec _ input = [(readFrom input, [])]

instance Show Program where
  show (Program instructions) = intercalate "," . map show $ instructions

readFrom :: String -> Program
readFrom = Program . map read . filter (not . null) . lines

current :: Program -> Instruction
current (Program instructions) = head instructions

remaining :: Program -> Program
remaining (Program instructions) = Program . tail $ instructions

isFinished :: Program -> Bool
isFinished (Program instructions) = null instructions
