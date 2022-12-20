module Day13.Signal (Signal, group, value, parseGroup) where

import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
    (<|>),
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( between,
    char,
    digit,
    many1,
    parse,
    sepBy,
  )

-- ---------------------------------------------------------------------------------------------------------------------
-- Signal data structure
-- ---------------------------------------------------------------------------------------------------------------------

data Signal = Value Int | Group [Signal] deriving (Eq, Show)

group :: [Signal] -> Signal
group = Group

value :: Int -> Signal
value = Value

-- ---------------------------------------------------------------------------------------------------------------------
-- Ord instance
-- ---------------------------------------------------------------------------------------------------------------------

instance Ord Signal where
  compare (Value x) (Value y) = compare x y
  compare leftHand@(Group _) rightHand@(Value _) = compare leftHand (Group [rightHand])
  compare leftHand@(Value _) rightHand@(Group _) = compare (Group [leftHand]) rightHand
  compare (Group x) (Group y) = compare x y

-- ---------------------------------------------------------------------------------------------------------------------
-- Read instance
-- ---------------------------------------------------------------------------------------------------------------------

instance Read Signal where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = toSignal . parse $ input

      toSignal :: Either ParseError Signal -> Signal
      toSignal (Right signal) = signal
      toSignal (Left parseError) = error . show $ parseError

-- ---------------------------------------------------------------------------------------------------------------------
-- Parsec Signal Parser
-- ---------------------------------------------------------------------------------------------------------------------

parseInt :: GenParser Char st Int
parseInt = read <$> Parsec.many1 Parsec.digit

parseValue :: GenParser Char st Signal
parseValue = Value <$> parseInt

parseSignals :: GenParser Char st [Signal]
parseSignals = Parsec.sepBy parseGroupOrValue (Parsec.char ',')

parseGroupOrValue :: GenParser Char st Signal
parseGroupOrValue = parseValue <|> parseGroup

parseGroupContent :: GenParser Char st [Signal]
parseGroupContent = parseSignals <|> return []

parseGroup :: GenParser Char st Signal
parseGroup = Group <$> Parsec.between (Parsec.char '[') (Parsec.char ']') parseGroupContent

parse :: String -> Either ParseError Signal
parse = Parsec.parse parseGroup "(unknown)"
