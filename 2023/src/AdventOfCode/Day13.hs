module AdventOfCode.Day13 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as Vector

type Pattern = Grid Vector Char

solution :: Solution
solution =
  Solution
    { parser = Grid.parse `sepEndBy'` Parser.endOfLine,
      solver = coerce . foldMap' summarize
    }

summarize :: Pattern -> (Sum Int, Sum Int)
summarize p = (Sum (score 0), Sum (score 1))
  where
    horizontal = zip (reflections (Grid.rows p)) [100, 200 ..]
    vertical = zip (reflections (Grid.cols p)) [1 ..]
    score d =
      fromMaybe (error "no reflections") $
        lookup d (horizontal <> vertical)

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
