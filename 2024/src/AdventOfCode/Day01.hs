module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntMap.Strict qualified as IntMap
import Data.Monoid (Sum (..))

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser (Int, Int)
parseLine = do
  x <- Parser.int
  Parser.whitespace
  y <- Parser.int
  pure (x, y)

solve :: [(Int, Int)] -> (Int, Int)
solve lists =
  ( sum $ zipWith (\x y -> abs (x - y)) (unfoldMap xs) (unfoldMap ys),
    getSum $
      IntMap.foldMapWithKey
        (\x n -> Sum (n * x * (IntMap.findWithDefault 0 x ys)))
        xs
  )
  where
    (xs, ys) = foldl' insert mempty lists
    insert (mx, my) (x, y) =
      (IntMap.insertWith (+) x 1 mx, IntMap.insertWith (+) y 1 my)
    unfoldMap = IntMap.foldrWithKey (\k n ns -> replicate n k <> ns) []
