module AdventOfCode.Day21 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search (bfsOnInt)
import Control.Exception (assert)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve1 &&& solve2
    }

solve1 :: Grid Vector Char -> Int
solve1 grid =
  count ((== parity) . even) $
    map fst $
      walk grid start steps
  where
    steps = 64
    parity = even steps
    start = fromMaybe (error "no start") $ Grid.findPosition (== 'S') grid

walk :: Grid Vector Char -> Position -> Int -> [(Int, Position)]
walk grid start steps = bfsOnInt hash (uncurry next) [(0, start)]
  where
    hash (_, Position {row, col}) = row * Grid.ncols grid + col
    next !n pos
      | n >= steps = []
      | otherwise = do
          dir <- [North, East, South, West]
          let p = Position.move dir pos
          guard (maybe False (/= '#') $ Grid.index grid p)
          pure (n + 1, p)

solve2 :: Grid Vector Char -> Int
solve2 grid =
  assert (nrows == ncols && even maps && 2 * rest + 1 == nrows) $
    square (maps - 1) * odds
      + square maps * evens
      + sides
      + maps * smallCorners
      + (maps - 1) * largeCorners
  where
    square x = x * x
    (nrows, ncols) = Grid.size grid
    steps = 26501365
    (maps, rest) = steps `divMod` nrows
    parity n = if even n then (1, 0) else (0, 1)
    fill = walk grid (Position {row = rest, col = rest}) (nrows + ncols)
    (Sum evens, Sum odds) = foldMap' parity $ map fst fill
    corners = [Position {row, col} | row <- [0, nrows - 1], col <- [0, ncols - 1]]
    countParity p n s = count (p . fst) (walk grid s n)
    Sum largeCorners = foldMap' (Sum . countParity odd (ncols + rest - 1)) corners
    Sum smallCorners = foldMap' (Sum . countParity even (rest - 1)) corners
    Sum sides =
      foldMap'
        (Sum . countParity even (ncols - 1))
        [ Position {row = 0, col = rest},
          Position {row = rest, col = 0},
          Position {row = nrows - 1, col = rest},
          Position {row = rest, col = ncols - 1}
        ]
