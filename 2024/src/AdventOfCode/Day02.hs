module AdventOfCode.Day02 where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Sum (..))

solution :: Solution
solution =
  Solution
    { parser = parseReport `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseReport :: Parser (NonEmpty Int)
parseReport = Parser.decimal `sepEndBy1'` Parser.whitespace

solve :: [NonEmpty Int] -> (Int, Int)
solve reports = (p1, p2)
  where
    (Sum p1, Sum p2) = foldMap' (go . NonEmpty.toList) reports
    go r
      | safe r = (1, 1)
      | any safe (dropLevel r) = (0, 1)
      | otherwise = (0, 0)

safe :: [Int] -> Bool
safe = \case
  [] -> True
  (_ : []) -> True
  (x : y : ys)
    | inc (y - x) -> all inc $ zipWith (-) ys (y : ys)
    | inc (x - y) -> all inc $ zipWith (-) (y : ys) ys
    | otherwise -> False
  where
    inc d = 1 <= d && d <= 3

dropLevel :: [a] -> [[a]]
dropLevel ls = map (`dropIndex` ls) [0 .. length ls - 1]

dropIndex :: Int -> [a] -> [a]
dropIndex !i = \case
  [] -> []
  (x : xs)
    | i == 0 -> xs
    | otherwise -> x : dropIndex (i - 1) xs
