module AdventOfCode.Day05 where

import AdventOfCode.Interval (Interval)
import AdventOfCode.Interval qualified as Interval
import AdventOfCode.Parser qualified as Parer
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (Map)
import Data.Char (isAsciiLower)

type Range = Interval Int

type Map = [(Range, Int)]

solution :: Solution
solution =
  Solution
    { parser = parseInput,
      part1 = uncurry (solve . ranges1),
      part2 = uncurry (solve . ranges2)
    }

parseInput :: Parser ([(Int, Int)], [Map])
parseInput = do
  seeds <- Parser.symbol "seeds:" *> pair `sepBy` Parser.whitespace
  Parser.skipSpace
  maps <- parseMap `sepEndBy` Parer.endOfLine
  pure (seeds, maps)
  where
    pair = do
      x <- Parser.decimal
      Parser.whitespace
      y <- Parser.decimal
      pure (x, y)

parseMap :: Parser Map
parseMap = do
  _name <- Parser.lexeme $ Parser.takeWhile1 (\c -> isAsciiLower c || c == '-')
  Parser.symbol "map:" >> Parser.endOfLine
  parseMapLine `sepEndBy'` Parser.endOfLine

parseMapLine :: Parser (Range, Int)
parseMapLine = do
  destination <- Parser.lexeme Parser.decimal
  source <- Parser.lexeme Parser.decimal
  len <- Parser.lexeme Parser.decimal
  let range = Interval.make source (source + len)
      shift = destination - source
  pure (range, shift)

solve :: [Range] -> [Map] -> Int
solve ranges =
  minimum
    . map Interval.start
    . foldl' (\rs m -> concatMap (applyMap m) rs) ranges

applyMap :: Map -> Range -> [Range]
applyMap = foldl' go f
  where
    f range = [range | not (Interval.null range)]
    go k (r, shift) range
      | Interval.null range = []
      | Interval.null intersection = k range
      | otherwise = Interval.shift shift intersection : k left <> k right
      where
        intersection = Interval.intersection range r
        (left, right) = Interval.difference range r

ranges1 :: [(Int, Int)] -> [Range]
ranges1 = concatMap (\(x, y) -> [Interval.make x (x + 1), Interval.make y (y + 1)])

ranges2 :: [(Int, Int)] -> [Range]
ranges2 = map (\(x, y) -> Interval.make x (x + y))
