module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = parseCard `sepEndBy'` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

data Card = Card
  { cardId :: !Int,
    winning :: [Int],
    numbers :: [Int]
  }
  deriving (Show)

parseCard :: Parser Card
parseCard = do
  Parser.symbol "Card"
  cardId <- Parser.decimal
  Parser.symbol ":"
  winning <- Parser.int `sepEndBy'` Parser.takeWhile1 (== ' ')
  Parser.symbol "|"
  numbers <- Parser.int `sepEndBy'` Parser.takeWhile1 (== ' ')

  pure Card {cardId, winning, numbers}

solve1 :: [Card] -> Int
solve1 = sum . map score
  where
    score c = 2 ^ matches c `div` 2

matches :: Card -> Int
matches Card {winning, numbers} = IntSet.size $ IntSet.fromList numbers `IntSet.intersection` IntSet.fromList winning

solve2 :: [Card] -> Int
solve2 cards = Vector.sum $ foldl' go copies $ zip [0 ..] $ map matches cards
  where
    copies = Vector.replicate (length cards) 1 :: Vector Int
    go cs (i, m) = Vector.accum (+) cs [(j, cs Vector.! i) | j <- [i + 1 .. i + m]]
