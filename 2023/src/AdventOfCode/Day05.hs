module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parer
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (Map)
import Data.Char (isAsciiLower)
import Data.Monoid (Endo (..))
import Debug.Trace (trace)

solution :: Solution
solution =
  Solution
    { parser = parseInput,
      part1 = uncurry solve1,
      part2 = uncurry solve2
    }

type Map = Int -> Int

type Input = ([Int], [Map])

parseInput :: Parser Input
parseInput = do
  seeds <- Parser.symbol "seeds:" *> Parser.decimal `sepBy` Parser.whitespace
  Parser.skipSpace
  maps <- parseMap `sepEndBy` Parer.endOfLine
  pure (seeds, maps)

parseMap :: Parser Map
parseMap = do
  _name <- Parser.lexeme $ Parser.takeWhile1 (\c -> isAsciiLower c || c == '-')
  Parser.symbol "map:" >> Parser.endOfLine
  ms <- parseMapLine `sepEndBy` Parser.endOfLine
  pure $ foldl' (flip ($)) id ms

parseMapLine :: Parser (Map -> Map)
parseMapLine = do
  destination <- Parser.lexeme Parser.decimal
  source <- Parser.lexeme Parser.decimal
  range <- Parser.lexeme Parser.decimal
  pure $ \k x ->
    let d = x - source
     in if d >= 0 && d < range
          then destination + d
          else k x

solve1 :: [Int] -> [Map] -> Int
solve1 seeds maps = minimum $ map f seeds
  where
    f x = foldl' (flip ($)) x maps

solve2 :: [Int] -> [Map] -> Int
solve2 seeds = solve1 (ranges seeds)
  where
    ranges = \case
      (x : y : ys) -> [x .. x + y] <> ys
      [_] -> []
      [] -> []
