module AdventOfCode.Day13 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List qualified as List
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as Vector

type Pattern = Grid Vector Char

solution :: Solution
solution =
  Solution
    { parser = Grid.parse `sepEndBy'` Parser.endOfLine,
      part1 = solve 0,
      part2 = solve 1
    }

-- | Number of different elements for each reflection line.
reflections :: (Eq a, Unbox a) => [Vector a] -> [Int]
reflections = \case
  [] -> []
  x : xs -> go xs [x]
  where
    go [] _ = []
    go (x : xs) ys =
      let d = sum $ zipWith diff (x : xs) ys
       in d : go xs (x : ys)

-- | Number of different elements between two vectors.
diff :: (Eq a, Unbox a) => Vector a -> Vector a -> Int
diff xs ys =
  Vector.sum $
    Vector.zipWith (\x y -> if x == y then 0 else 1) xs ys

solve :: Int -> [Pattern] -> Int
solve d = sum . mapMaybe notes
  where
    notes pattern = horizontal pattern <|> vertical pattern
    horizontal = fmap (* 100) . refs . Grid.rows
    vertical = refs . Grid.cols
    refs = fmap (+ 1) . List.elemIndex d . reflections
