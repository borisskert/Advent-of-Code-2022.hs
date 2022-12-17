module Day11.WorryLevel (WorryLevel) where

newtype WorryLevel = WorryLevel Int deriving (Show, Eq, Ord)

instance Num WorryLevel where
  (+) (WorryLevel a) (WorryLevel b) = WorryLevel (a + b)
  (*) (WorryLevel a) (WorryLevel b) = WorryLevel (a * b)
  abs (WorryLevel a) = WorryLevel (abs a)
  signum (WorryLevel a) = WorryLevel . signum $ a
  fromInteger i = WorryLevel . fromInteger $ i
  negate (WorryLevel a) = WorryLevel (negate a)

instance Real WorryLevel where
  toRational = error "WorryLevel.toRational not implemented"

instance Enum WorryLevel where
  toEnum = WorryLevel
  fromEnum (WorryLevel i) = i

instance Integral WorryLevel where
  quotRem (WorryLevel a) (WorryLevel b) = (WorryLevel q, WorryLevel r)
    where
      (q, r) = quotRem a b
  toInteger = error "WorryLevel.toInteger not implemented"

instance Read WorryLevel where
  readsPrec _ input = [(WorryLevel parsed, [])]
    where
      parsed = read input
