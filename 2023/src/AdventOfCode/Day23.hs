module AdventOfCode.Day23 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (invert)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.HashSet qualified as HashSet
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      part1 = solve1,
      part2 = solve2
    }

solve1 :: Grid Vector Char -> Int
solve1 = walk step
  where
    step = \case
      '.' -> [North, East, South, West]
      '^' -> [North]
      '>' -> [East]
      'v' -> [South]
      '<' -> [West]
      _ -> []

solve2 :: Grid Vector Char -> Int
solve2 = walk step
  where
    step = \case
      '#' -> []
      _ -> [North, East, South, West]

walk :: (Char -> [Direction]) -> Grid Vector Char -> Int
walk step grid = go 0 HashSet.empty (start, South)
  where
    start = fromMaybe (error "no start") $ Grid.findPosition (== '.') grid
    go :: Int -> HashSet Position -> (Position, Direction) -> Int
    go !n seen (pos, dir)
      | row pos == Grid.nrows grid - 1 = n
      | otherwise = case next of
          [] -> 0
          [(p, d)] -> go (n + 1) seen (p, d)
          ns -> maximum $ mapper (go (n + 1) (HashSet.insert pos seen)) ns
      where
        mapper = if n < 2048 then parMap rseq else map
        next = do
          d <- step (Grid.unsafeIndex grid pos)
          guard (d /= invert dir) -- make sure we don't go back
          let p = Position.move d pos
          case Grid.index grid p of
            Nothing -> []
            Just t
              | t == '#' || p `HashSet.member` seen -> []
              | otherwise -> pure (p, d)
