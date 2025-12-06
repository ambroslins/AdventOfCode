module AdventOfCode.Day02 where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))

solution :: Solution
solution =
  Solution
    { parser = (parseRange `sepBy'` Parser.char ',') <* Parser.endOfLine,
      solver = solve
    }

parseRange :: Parser (Int, Int)
parseRange = do
  !start <- Parser.decimal
  _ <- Parser.char '-'
  !end <- Parser.decimal
  return (start, end)

solve :: [(Int, Int)] -> (Int, Int)
solve = coerce . foldMap' go
  where
    go range =
      let part1 =
            countInvalidIds 2 2 range
              + countInvalidIds 4 2 range
              + countInvalidIds 6 2 range
              + countInvalidIds 8 2 range
              + countInvalidIds 10 2 range
          part2 =
            part1
              + countInvalidIds 3 3 range
              + countInvalidIds 5 5 range
              + countInvalidIds 6 3 range
              + countInvalidIds 7 7 range
              + countInvalidIds 9 3 range
              + countInvalidIds 10 5 range
              - countInvalidIds 6 6 range
              - countInvalidIds 10 10 range
       in (Sum part1, Sum part2)

countInvalidIds :: Int -> Int -> (Int, Int) -> Int
countInvalidIds digits reps (start, end)
  | n > m = 0
  | otherwise = pattern * (1 + m - n) * (n + m) `div` 2
  where
    base = 10 ^ (digits `div` reps)
    pattern = (10 ^ digits - 1) `div` (base - 1)
    lower = base `div` 10
    upper = base - 1
    n =
      max lower $
        let (d, rest) = start `divMod` pattern
         in if rest == 0 then d else d + 1
    m = min upper $ end `div` pattern
