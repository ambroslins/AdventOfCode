module AdventOfCode.Day17 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (turnLeft, turnRight)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
import Data.Char (ord)
import Data.List qualified as List
import Data.Vector.Unboxed (Vector)

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
  fromMaybe (error "no path found") $
    findFirst (\(loss, (pos, _)) -> if pos == end then Just loss else Nothing) $
      Search.dijkstraOnN id next [(0, start East), (0, start South)]
  where
    start dir = (Position.origin, dir)
    end = Position {row = Grid.nrows grid - 1, col = Grid.ncols grid - 1}
    next loss (pos, dir) = do
      (p, l) <- drop (minStraight - 1) $ zip ps ls
      [(l, (p, turnLeft dir)), (l, (p, turnRight dir))]
      where
        ps = take maxStraight $ drop 1 $ iterate (Position.move dir) pos
        ls = drop 1 $ List.scanl' (+) loss $ mapMaybe (Grid.index grid) ps

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst f = go
  where
    go = \case
      [] -> Nothing
      x : xs -> f x <|> go xs
