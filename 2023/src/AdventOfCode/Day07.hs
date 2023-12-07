module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlphaNum)
import Data.Foldable (fold)
import Data.Ord (comparing)

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
    cards :: !Cards
  }
  deriving (Show, Eq)

instance Ord Hand where
  compare = comparing kind <> (compareCards `on` cards)
    where
      compareCards cs1 cs2 = fold $ BS.zipWith compareCard cs1 cs2

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      part1 = solve makeHand,
      part2 = solve makeHandWithJoker
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
makeHand cards = Hand {kind = handKind 0 cards, cards}

makeHandWithJoker :: Cards -> Hand
makeHandWithJoker cs =
  Hand
    { kind = handKind (BS.count 'J' cs) (BS.filter (/= 'J') cs),
      cards = BS.map (\c -> if c == 'J' then '1' else c) cs
    }

compareCard :: Card -> Card -> Ordering
compareCard =
  foldMap comparing [(== 'A'), (== 'K'), (== 'Q'), (== 'J'), (== 'T')]
    <> compare

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
