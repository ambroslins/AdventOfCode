module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List qualified as List

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseLine = do
  x <- Parser.int
  Parser.whitespace
  y <- Parser.int
  pure (x, y)

solve1 ls = sum $ map (\(x, y) -> abs (x - y)) $ zip (List.sort xs) (List.sort ys)
  where
    (xs, ys) = unzip ls

solve2 ls = sum $ map (\x -> x * count (== x) ys) xs
  where
    (xs, ys) = unzip ls
