module AdventOfCode.Day08 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid = (antinodes [1], antinodes [0 .. 100])
  where
    positions = Grid.findPositions (/= '.') grid
    m =
      Map.fromListWith (<>) $
        [ (Grid.unsafeIndex grid p, [p])
        | p <- Vector.toList positions
        ]
    antinodes range =
      Set.size $ Set.fromList $ filter (`Grid.inside` grid) $ do
        (_, ps) <- Map.toList m
        Position row1 col1 <- ps
        Position row2 col2 <- ps
        let dr = row2 - row1
            dc = col2 - col1
        guard (dr /= 0 || dc /= 0)
        n <- range
        [Position (row1 - n * dr) (col1 - n * dc), Position (row2 + n * dr) (col2 + n * dc)]
