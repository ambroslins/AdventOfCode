module AdventOfCode.Day10 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Data.Char qualified as Char
import Data.IntSet qualified as IntSet
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = Grid.map (\c -> Char.ord c - Char.ord '0') <$> Grid.parse,
      solver = solve
    }

solve :: Grid Vector Int -> (Int, Int)
solve grid = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) = Vector.foldMap' go starts
    starts = Grid.findPositions (== 0) grid
    go start =
      let Pair ends distinct = walk (Pair IntSet.empty 0) (Pair 0 start)
       in Pair (Sum $ IntSet.size ends) (Sum distinct)

    walk (Pair ends distinct) (Pair height pos)
      | height == 9 = Pair (IntSet.insert (hash pos) ends) (distinct + 1)
      | otherwise = foldl' walk (Pair ends distinct) (steps height pos)

    steps !height !pos =
      let !up = height + 1
       in [ Pair up next
          | dir <- [North, East, South, West],
            let next = Pos.move dir pos,
            Grid.index grid next == Just up
          ]

    ncols = Grid.ncols grid
    hash Position {row, col} = row * ncols + col
