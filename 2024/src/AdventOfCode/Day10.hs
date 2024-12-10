module AdventOfCode.Day10 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Debug.Trace (traceShow)
import Deque.Strict qualified as Deque
import GHC.IsList (fromList)

solution :: Solution
solution =
  Solution
    { parser = Grid.map (\c -> Char.ord c - Char.ord '0') <$> Grid.parse,
      solver = solve
    }

solve :: Grid Vector Int -> (Int, Int)
solve grid =
  ( Vector.sum $ Vector.map (count ((== 9) . fst) . trailheads) starts,
    Vector.sum $ Vector.map (solve2) starts
  )
  where
    starts = Grid.findPositions (== 0) grid
    trailheads start =
      Search.bfsOnInt
        hash
        neighbours
        [(0, start)]

    solve2 start =
      distinctPaths ((== 9) . fst) neighbours (0, start)

    ncols = Grid.ncols grid
    hash (height, Position {row, col}) = (row * ncols + col) * 10 + height
    neighbours (height, pos) =
      [ (fromJust nextHeight, next)
      | dir <- [North, East, South, West],
        let next = Pos.move dir pos,
        let nextHeight = Grid.index grid next,
        nextHeight == Just (height + 1)
      ]

distinctPaths :: (a -> Bool) -> (a -> [a]) -> a -> Int
distinctPaths isEnd children = go
  where
    go start
      | isEnd start = 1
      | otherwise = sum $ (map go $ children start)
