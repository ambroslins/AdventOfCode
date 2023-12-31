module AdventOfCode.Day16 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.HashSet qualified as HashSet
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve1 &&& solve2
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
energizedTiles grid = HashSet.size . uncurry (go HashSet.empty)
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
          | pos `HashSet.member` energized -> energized
          | dir `elem` [North, South] -> goNext dir
          | otherwise -> go (goNext North) (Position.move South pos) South
        Just '-'
          | pos `HashSet.member` energized -> energized
          | dir `elem` [East, West] -> goNext dir
          | otherwise -> go (goNext East) (Position.move West pos) West
        Just c -> error $ "invalid tile: " <> show c
      where
        goNext d =
          go (HashSet.insert pos energized) (Position.move d pos) d
