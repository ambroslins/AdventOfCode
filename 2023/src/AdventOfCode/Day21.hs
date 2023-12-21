module AdventOfCode.Day21 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search (bfsOnN)
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      part1 = solve1,
      part2 = Grid.nrows
    }

solve1 :: Grid Vector Char -> Int
solve1 grid =
  length $
    takeWhile (== 64) $
      dropWhile (< 64) $
        map fst $
          bfsOnN id (uncurry steps) [(0, start)]
  where
    start = fromMaybe (error "no start") $ Grid.findPosition (== 'S') grid
    steps :: Int -> Position -> [(Int, Position)]
    steps !n pos = do
      dir <- [North, East, South, West]
      let p = Position.move dir pos
      guard (maybe False (/= '#') $ Grid.index grid p)
      pure (n + 1, p)
