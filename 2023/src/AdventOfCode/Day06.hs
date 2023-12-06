module AdventOfCode.Day06 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bifunctor (bimap)

solution :: Solution
solution =
  Solution
    { parser = do
        times <- parseTimes <* Parser.endOfLine
        distances <- parseDistances <* Parser.endOfLine
        pure $ zip times distances,
      part1 = solve1,
      part2 = solve2
    }

parseTimes :: Parser [Int]
parseTimes = Parser.symbol "Time:" *> Parser.decimal `sepBy` Parser.whitespace

parseDistances :: Parser [Int]
parseDistances = Parser.symbol "Distance:" *> Parser.decimal `sepBy` Parser.whitespace

solve1 :: [(Int, Int)] -> Int
solve1 = product . map (uncurry wins)

wins :: Int -> Int -> Int
wins time distance = count p [1 .. time]
  where
    p a = a * (time - a) > distance

removeSpaces :: [Int] -> Int
removeSpaces = read . concatMap show

solve2 :: [(Int, Int)] -> Int
solve2 = uncurry wins . convert
  where
    convert = bimap removeSpaces removeSpaces . unzip
