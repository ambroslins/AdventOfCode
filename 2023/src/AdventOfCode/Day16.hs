module AdventOfCode.Day16 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.IntSet qualified as IntSet
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid = (part1, part2)
  where
    part1 = energizedTiles grid (Position {row = 0, col = 0}, East)
    part2 = foldl' max part1 $ parMap rseq (energizedTiles grid) starts
    starts = left <> right <> top <> bottom
    top = [(Position {row = 0, col}, South) | col <- [0 .. c]]
    bottom = [(Position {row = r, col}, North) | col <- [0 .. c]]
    left = [(Position {row, col = 0}, East) | row <- [1 .. r]]
    right = [(Position {row, col = c}, West) | row <- [0 .. r]]
    r = Grid.nrows grid - 1
    c = Grid.ncols grid - 1

energizedTiles :: Grid Vector Char -> (Position, Direction) -> Int
energizedTiles grid = IntSet.size . uncurry (go IntSet.empty)
  where
    go !energized !pos !dir =
      case Grid.index grid pos of
        Nothing -> energized
        Just '.' -> goNext dir
        Just '/' -> goNext $ case dir of
          North -> East
          East -> North
          South -> West
          West -> South
        Just '\\' -> goNext $ case dir of
          North -> West
          East -> South
          South -> East
          West -> North
        Just '|'
          | r `IntSet.member` energized -> energized
          | dir `elem` [North, South] -> goNext dir
          | otherwise -> go (goNext North) (Position.move South pos) South
        Just '-'
          | r `IntSet.member` energized -> energized
          | dir `elem` [East, West] -> goNext dir
          | otherwise -> go (goNext East) (Position.move West pos) West
        Just c -> error $ "invalid tile: " <> show c
      where
        r = row pos * Grid.ncols grid + col pos
        goNext d = go (IntSet.insert r energized) (Position.move d pos) d
