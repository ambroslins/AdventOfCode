module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Char (isDigit)
import Data.HashSet qualified as HashSet

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
  winning <- Parser.takeWhile1 isDigit `sepEndBy'` Parser.takeWhile1 (== ' ')
  Parser.symbol "|"
  numbers <- Parser.takeWhile1 isDigit `sepEndBy'` Parser.takeWhile1 (== ' ')
  let winningSet = HashSet.fromList winning

  pure $ count (`HashSet.member` winningSet) numbers

solve1 :: [Int] -> Int
solve1 = sum . map score
  where
    score m = 2 ^ max 0 (m - 1)

solve2 :: [Int] -> Int
solve2 = sum . foldr go []
  where
    go c cs = 1 + sum (take c cs) : cs
