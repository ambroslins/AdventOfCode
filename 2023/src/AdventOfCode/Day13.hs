module AdventOfCode.Day13 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Vector.Unboxed (Vector)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse `sepEndBy'` Parser.endOfLine,
      part1 = solve1,
      part2 = map Grid.ncols
    }

reflections :: [Vector Char] -> Maybe Int
reflections ls = go 0 ls []
  where
    go _ [] _ = Nothing
    go !i (x : xs) ys
      | null ys = go (i + 1) xs [x]
      | doesReflect = Just i
      | otherwise = go (i + 1) xs (x : ys)
      where
        doesReflect = and $ zipWith (==) (x : xs) ys

solve1 :: [Grid Vector Char] -> Int
solve1 = sum . map (\grid -> horizontal grid + vertical grid)
  where
    horizontal = maybe 0 (* 100) . reflections . Grid.rows
    vertical = fromMaybe 0 . reflections . Grid.cols
