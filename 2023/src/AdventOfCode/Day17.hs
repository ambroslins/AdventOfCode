module AdventOfCode.Day17 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (turnLeft, turnRight)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
import Data.Char (ord)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = Grid.map digitToInt <$> Grid.parse,
      solver = solve 1 3 &&& solve 4 10
    }

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

solve :: Int -> Int -> Grid Vector Int -> Int
solve minStraight maxStraight grid =
  fromMaybe (error "no path found")
    . findFirst (\(loss, (pos, _)) -> if pos == end then Just loss else Nothing)
    $ Search.dijkstraOnInt
      (perfectHash $ Grid.ncols grid)
      next
      [(0, start East), (0, start South)]
  where
    start dir = (Position.origin, dir)
    end = Position {row = Grid.nrows grid - 1, col = Grid.ncols grid - 1}
    next !loss (!pos, !dir) = do
      d <- [turnLeft dir, turnRight dir]
      let move = Position.move d
          ps =
            Vector.takeWhile (`Grid.inside` grid) $
              Vector.iterateN maxStraight move (move pos)
          ls =
            Vector.postscanl' (+) loss $
              Vector.map (Grid.unsafeIndex grid) ps
      (p, l) <- Vector.toList $ Vector.drop (minStraight - 1) $ Vector.zip ps ls
      pure (l, (p, d))
{-# INLINE solve #-}

-- | Perfect hash function for positions and directions.
-- Because we turn left and right at each step we only care about
-- facing horizontally or vertically.
perfectHash :: Int -> (Position, Direction) -> Int
perfectHash ncols (Position {row, col}, dir) =
  (row * ncols + col) * 2 + case dir of
    North -> 1
    East -> 0
    South -> 1
    West -> 0
