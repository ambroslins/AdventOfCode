module AdventOfCode.Day06 (solution, wins) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bifunctor (bimap)
import GHC.Float (floorDouble)

solution :: Solution
solution =
  Solution
    { parser = do
        times <- parseTimes <* Parser.endOfLine
        distances <- parseDistances <* Parser.endOfLine
        pure (times, distances),
      part1 = solve1,
      part2 = solve2
    }

parseTimes :: Parser [Int]
parseTimes = Parser.symbol "Time:" *> Parser.decimal `sepBy` Parser.whitespace

parseDistances :: Parser [Int]
parseDistances = Parser.symbol "Distance:" *> Parser.decimal `sepBy` Parser.whitespace

solve1 :: ([Int], [Int]) -> Int
solve1 = product . map (uncurry wins) . uncurry zip

wins :: Int -> Int -> Int
wins time distance = x2 - x1
  where
    -- p(x) = a*x^2 + b*x + c, b = -t, a = 1
    t = fromIntegral time
    c = fromIntegral distance + 0.5
    d = t * t - 4 * c
    s = sqrt d
    x1 = floorDouble $ (t - s) / 2
    x2 = floorDouble $ (t + s) / 2

removeSpaces :: [Int] -> Int
removeSpaces = read . concatMap show

solve2 :: ([Int], [Int]) -> Int
solve2 = uncurry wins . bimap removeSpaces removeSpaces
