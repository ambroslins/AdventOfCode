module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed qualified as Vector

type Rule = (Int, Int)

type Update = [Int]

solution :: Solution
solution =
  Solution
    { parser = do
        rules <- parseRule `sepEndBy'` Parser.endOfLine
        Parser.endOfLine
        updates <- parseUpdate `sepEndBy'` Parser.endOfLine
        pure (rules, updates),
      solver = uncurry solve
    }

parseRule :: Parser Rule
parseRule = do
  !x <- Parser.decimal
  _ <- Parser.char '|'
  !y <- Parser.decimal
  pure $ (x, y)

parseUpdate :: Parser Update
parseUpdate = (NonEmpty.toList) <$> (Parser.decimal `sepBy1` Parser.char ',')

solve :: [Rule] -> [Update] -> (Int, Int)
solve rules updates = coerce $ foldMap' go updates
  where
    go update
      | update == sorted = (Sum mid, 0)
      | otherwise = (0, Sum mid)
      where
        sorted = List.sortBy comp update
        !mid = sorted !! (length update `div` 2)
    vec =
      Vector.replicate (100 * 100) False
        Vector.// [(perfectHash low high, True) | (low, high) <- rules]
    comp x y
      | Vector.unsafeIndex vec (perfectHash x y) = LT
      | otherwise = GT

perfectHash :: Int -> Int -> Int
perfectHash low high = low * 100 + high
