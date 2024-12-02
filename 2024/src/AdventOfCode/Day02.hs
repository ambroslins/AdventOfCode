module AdventOfCode.Day02 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List.NonEmpty (toList)

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseLine :: Parser (NonEmpty Int)
parseLine = Parser.int `sepEndBy1` Parser.whitespace

solve1 = count safe . map toList
  where
    safe ls =
      let diff = diffs ls
       in all (\d -> d >= 1 && d <= 3) diff
            || all (\d -> d <= -1 && d >= -3) diff

solve2 = count (any safe . dropLevels) . map toList
  where
    safe ls =
      let diff = diffs ls
       in all (\d -> d >= 1 && d <= 3) diff
            || all (\d -> d <= -1 && d >= -3) diff

diffs (x : xs) = go x xs
  where
    go y (z : zs) = (z - y) : go z zs
    go _ [] = []

dropLevels :: [a] -> [[a]]
dropLevels (x : xs) = xs : map (x :) (dropLevels xs)
dropLevels [] = [[]]
