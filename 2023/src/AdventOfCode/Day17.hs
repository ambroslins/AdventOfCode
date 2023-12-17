module AdventOfCode.Day17 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (turnLeft, turnRight)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
import Data.Char (ord)
import Data.Vector.Unboxed (Vector)

type TurnRule = Direction -> Int -> [(Direction, Int)]

solution :: Solution
solution =
  Solution
    { parser = Grid.map digitToInt <$> Grid.parse,
      part1 = solve 1 largeCrucible,
      part2 = solve 4 ultraCrucible
    }

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

solve :: Int -> TurnRule -> Grid Vector Int -> Int
solve stop turn grid =
  head
    $ mapMaybe
      ( \(loss, (pos, _, n)) ->
          if pos == end && n >= stop
            then Just loss
            else Nothing
      )
    $ Search.dijkstraOnN id next [(0, start East), (0, start South)]
  where
    start dir = (Position.origin, dir, 0)
    end = Position {row = Grid.nrows grid - 1, col = Grid.ncols grid - 1}
    next loss (pos, dir, n) = do
      (d, m) <- turn dir n
      let p = Position.move d pos
      case Grid.index grid p of
        Nothing -> []
        Just l -> pure (loss + l, (p, d, m + 1))

largeCrucible :: TurnRule
largeCrucible dir n
  | n < 3 = (dir, n) : turns
  | otherwise = turns
  where
    turns = [(turnLeft dir, 0), (turnRight dir, 0)]

ultraCrucible :: TurnRule
ultraCrucible dir n
  | n < 4 = [straight]
  | n < 10 = straight : turns
  | otherwise = turns
  where
    straight = (dir, n)
    turns = [(turnLeft dir, 0), (turnRight dir, 0)]
