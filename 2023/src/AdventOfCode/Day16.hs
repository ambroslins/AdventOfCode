module AdventOfCode.Day16 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
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
solve1 grid = energizedTiles grid (Position {row = 0, col = 0}, East)

solve2 :: Grid Vector Char -> Int
solve2 grid = maximum $ parMap rseq (energizedTiles grid) starts
  where
    starts = top <> bottom <> left <> right
    top = [(Position {row = 0, col}, South) | col <- [0 .. c]]
    bottom = [(Position {row = r, col}, North) | col <- [0 .. c]]
    left = [(Position {row, col = 0}, East) | row <- [0 .. r]]
    right = [(Position {row, col = c}, West) | row <- [0 .. r]]
    r = Grid.nrows grid - 1
    c = Grid.ncols grid - 1

energizedTiles :: Grid Vector Char -> (Position, Direction) -> Int
energizedTiles grid =
  HashSet.size . HashSet.fromList . map fst . Search.dfs move
  where
    move (position, direction) = do
      dir <- case Grid.unsafeIndex grid position of
        '.' -> [direction]
        '/' -> case direction of
          North -> [East]
          East -> [North]
          South -> [West]
          West -> [South]
        '\\' -> case direction of
          North -> [West]
          East -> [South]
          South -> [East]
          West -> [North]
        '|' -> case direction of
          North -> [North]
          East -> [North, South]
          South -> [South]
          West -> [North, South]
        '-' -> case direction of
          North -> [East, West]
          East -> [East]
          South -> [East, West]
          West -> [West]
        c -> error $ "invalid tile: " <> show c
      let pos = Position.move dir position
      guard $ Grid.inside pos grid
      pure (pos, dir)
