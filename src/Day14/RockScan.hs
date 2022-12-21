module Day14.RockScan (RockScan, fromList, toList, parseOne, deflate) where

import Common.List (distinct, range)
import Common.Read
import Control.Monad (void)
import Data.List.Split (divvy)
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( char,
    digit,
    many1,
    parse,
    sepBy1,
    string,
  )

-- | -------------------------------------------------------------------------------------------------------------------
-- | A RockScan is a scan of a rock.
-- | -------------------------------------------------------------------------------------------------------------------
newtype RockScan = RockScan [(Int, Int)] deriving (Show, Eq)

fromList :: [(Int, Int)] -> RockScan
fromList = RockScan

toList :: RockScan -> [(Int, Int)]
toList (RockScan pairs) = pairs

deflate :: RockScan -> [(Int, Int)]
deflate (RockScan pairs) = distinct . concatMap deflatePair . divvy 2 1 $ pairs

deflatePair :: [(Int, Int)] -> [(Int, Int)]
deflatePair [(x1, y1), (x2, y2)]
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = error "deflatePair: not a horizontal or vertical line"
deflatePair _ = error "deflatePair: not a pair"

-- | -------------------------------------------------------------------------------------------------------------------
-- | Read instance
-- | -------------------------------------------------------------------------------------------------------------------
instance Read RockScan where
  readsPrec _ = readBy parse

-- | -------------------------------------------------------------------------------------------------------------------
-- | Parsec RockScan Parser
-- | -------------------------------------------------------------------------------------------------------------------
parse :: String -> Either ParseError RockScan
parse = Parsec.parse parseOne "(ParseError while parsing RockScan)"

parseOne :: GenParser Char st RockScan
parseOne = RockScan <$> manyTuples

separator :: GenParser Char st ()
separator = void (Parsec.string " -> ")

tuple :: GenParser Char st (Int, Int)
tuple = do
  x <- Parsec.many1 Parsec.digit
  _ <- Parsec.char ','
  y <- Parsec.many1 Parsec.digit
  return (read x, read y)

manyTuples :: GenParser Char st [(Int, Int)]
manyTuples = Parsec.sepBy1 tuple separator
