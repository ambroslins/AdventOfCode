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

type Hand = ByteString

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      part1 = solve compareHand,
      part2 = solve compareWithJoker
    }

parseLine :: Parser (Hand, Int)
parseLine = do
  cards <- Parser.takeWhile1 isAlphaNum
  Parser.whitespace
  bid <- Parser.decimal
  pure (cards, bid)

solve :: (Hand -> Hand -> Ordering) -> [(Hand, Int)] -> Int
solve cmp = sum . zipWith (*) [1 ..] . map snd . sortBy (cmp `on` fst)

compareCard :: Card -> Card -> Ordering
compareCard =
  fold
    [ comparing (== 'A'),
      comparing (== 'K'),
      comparing (== 'Q'),
      comparing (== 'J'),
      comparing (== 'T'),
      compare
    ]

compareHand :: Hand -> Hand -> Ordering
compareHand = compareKind <> compareCardsBy compareCard

compareWithJoker :: Hand -> Hand -> Ordering
compareWithJoker =
  compareKindWithJoker
    <> compareCardsBy (comparing (/= 'J') <> compareCard)

compareKind :: Hand -> Hand -> Ordering
compareKind = comparing (handKind 0)

compareKindWithJoker :: Hand -> Hand -> Ordering
compareKindWithJoker = comparing kind
  where
    kind hand = handKind (BS.count 'J' hand) (BS.filter (/= 'J') hand)

compareCardsBy :: (Card -> Card -> Ordering) -> Hand -> Hand -> Ordering
compareCardsBy f h1 h2 = fold $ BS.zipWith f h1 h2

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
