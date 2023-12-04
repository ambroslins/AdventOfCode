module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntSet qualified as IntSet

solution :: Solution
solution =
  Solution
    { parser = parseCard `sepEndBy'` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseCard :: Parser Int
parseCard = do
  Parser.symbol "Card"
  _cardId <- Parser.decimal @Int
  Parser.symbol ":"
  winning <- Parser.int `sepEndBy'` Parser.takeWhile1 (== ' ')
  Parser.symbol "|"
  numbers <- Parser.int `sepEndBy'` Parser.takeWhile1 (== ' ')
  let numSet = IntSet.fromList winning

  pure $ count (`IntSet.member` numSet) numbers

solve1 :: [Int] -> Int
solve1 = sum . map score
  where
    score m = 2 ^ max 0 (m - 1)

solve2 :: [Int] -> Int
solve2 = sum . go . map (1,)
  where
    go [] = []
    go ((c, m) : xs) = c : go (addCopies c m xs)

    addCopies c m = \case
      (x : xs) | m > 0 -> first (+ c) x : addCopies c (m - 1) xs
      xs -> xs
