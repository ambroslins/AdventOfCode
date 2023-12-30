module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlphaNum)
import Data.Char qualified as Char

data Kind
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

type Card = Char

type Cards = ByteString

data Hand = Hand
  { kind :: !Kind,
    value :: !Int
  }
  deriving (Show, Eq, Ord)

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve makeHand &&& solve makeHandWithJoker
    }

parseLine :: Parser (Cards, Int)
parseLine = do
  cards <- Parser.takeWhile1 isAlphaNum
  Parser.whitespace
  bid <- Parser.decimal
  pure (cards, bid)

solve :: (Cards -> Hand) -> [(Cards, Int)] -> Int
solve make = sum . zipWith (*) [1 ..] . map snd . sortOn (make . fst)

makeHand :: Cards -> Hand
makeHand cards =
  Hand
    { kind = handKind 0 cards,
      value = cardsValue cardValue cards
    }

makeHandWithJoker :: Cards -> Hand
makeHandWithJoker cs =
  Hand
    { kind = handKind (BS.count 'J' cs) (BS.filter (/= 'J') cs),
      value = cardsValue (\c -> if c == 'J' then 1 else cardValue c) cs
    }

cardValue :: Card -> Int
cardValue = \case
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  c -> Char.ord c - Char.ord '0'

cardsValue :: (Card -> Int) -> Cards -> Int
cardsValue f = BS.foldl' (\acc c -> f c + 15 * acc) 0

handKind :: Int -> ByteString -> Kind
handKind jokers hand =
  case sortBy (flip compare) $ map BS.length $ BS.group $ BS.sort hand of
    []
      | jokers == 5 -> FiveOfAKind
      | otherwise -> error "handKind: empty hand"
    (c : cs) -> case (c + jokers, cs) of
      (5, []) -> FiveOfAKind
      (4, [1]) -> FourOfAKind
      (3, [2]) -> FullHouse
      (3, [1, 1]) -> ThreeOfAKind
      (2, [2, 1]) -> TwoPair
      (2, [1, 1, 1]) -> OnePair
      (1, [1, 1, 1, 1]) -> HighCard
      _ -> error $ "handKind: invalid kind: " <> show hand
