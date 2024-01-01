module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Char (isDigit)
import Data.IntSet qualified as IntSet

solution :: Solution
solution =
  Solution
    { parser = parseCard `sepEndBy'` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseCard :: Parser Int
parseCard = do
  Parser.symbol "Card"
  Parser.skipWhile isDigit
  Parser.symbol ":"
  winning <- Parser.decimal `sepEndBy'` Parser.takeWhile1 (== ' ')
  Parser.symbol "|"
  numbers <- Parser.decimal `sepEndBy'` Parser.takeWhile1 (== ' ')
  let winningSet = IntSet.fromList winning
  pure $ count (`IntSet.member` winningSet) numbers

solve1 :: [Int] -> Int
solve1 = sum . map score
  where
    score m = 2 ^ max 0 (m - 1)

solve2 :: [Int] -> Int
solve2 = sum . foldr go []
  where
    go !c cs = 1 + sum (take c cs) : cs
