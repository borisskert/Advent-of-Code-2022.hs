module Day14.RockScan (RockScan, fromList, parseOne) where

import Common.Read
import Control.Monad (void)
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
