module AdventOfCode.Day04 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Prelude
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) =
      Vector.foldMap' go $
        Grid.findPositions (== 'A') grid
    go p = Pair (Sum $ countXmas p) (if isX p then 1 else 0)

    deltas =
      [ Triple
          (Position (-2 * dr) (-2 * dc)) -- X
          (Position (-dr) (-dc)) --         M
          --                                A (skipped)
          (Position dr dc) --               S
      | dr <- [-1, 0, 1],
        dc <- [-1, 0, 1]
      ]

    countXmas p =
      count
        ( \(Triple dx dm ds) ->
            Grid.index grid (p <> dx) == Just 'X'
              && Grid.index grid (p <> dm) == Just 'M'
              && Grid.index grid (p <> ds) == Just 'S'
        )
        deltas

    isX (Position row col)
      | Just nw <- Grid.index grid (Position (row - 1) (col - 1)),
        Just ne <- Grid.index grid (Position (row - 1) (col + 1)),
        Just sw <- Grid.index grid (Position (row + 1) (col - 1)),
        Just se <- Grid.index grid (Position (row + 1) (col + 1)) =
          diag nw se && diag ne sw
      | otherwise = False

    diag x y = case x of
      'M' -> y == 'S'
      'S' -> y == 'M'
      _ -> False
