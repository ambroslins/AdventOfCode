module AdventOfCode.Day02 where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bits (shiftR)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = (parseRange `sepBy'` Parser.char ',') <* Parser.endOfLine,
      solver = solve invalidIds1 &&& solve invalidIds2
    }

parseRange :: Parser (Int, Int)
parseRange = do
  start <- Parser.decimal
  _ <- Parser.char '-'
  end <- Parser.decimal
  return (start, end)

solve :: Vector Int -> [(Int, Int)] -> Int
solve ids = getSum . foldMap' (Sum . go)
  where
    go (start, end) =
      let i = binarySearch start ids
          j = binarySearch (end + 1) ids
       in Vector.sum $ Vector.slice i (j - i) ids

invalidIds1 :: Vector Int
invalidIds1 =
  Vector.fromListN 99_999 $
    concat
      [ map (* 11) [1 .. 9],
        map (* 0_101) [10 .. 99],
        map (* 001_001) [100 .. 999],
        map (* 00_010_001) [1_000 .. 9_999],
        map (* 0_000_100_001) [10_000 .. 99_999]
      ]

invalidIds2 :: Vector Int
invalidIds2 =
  Vector.fromListN 101_088 $
    concat
      [ map (* 11) [1 .. 9],
        map (* 111) [1 .. 9],
        map (* 1_111) [1 .. 9] `merge` map (* 0_101) [10 .. 99],
        map (* 11_111) [1 .. 9],
        map (* 111_111) [1 .. 9] `merge` map (* 010_101) [10 .. 99] `merge` map (* 001_001) [100 .. 999],
        map (* 1_111_111) [1 .. 9],
        map (* 11_111_111) [1 .. 9] `merge` map (* 01_010_101) [10 .. 99] `merge` map (* 00_010_001) [1_000 .. 9_999],
        map (* 111_111_111) [1 .. 9] `merge` map (* 001_001_001) [100 .. 999],
        map (* 1_111_111_111) [1 .. 9] `merge` map (* 0_101_010_101) [10 .. 99] `merge` map (* 0_000_100_001) [10_000 .. 99_999]
      ]

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge a@(x : xs) b@(y : ys) = case compare x y of
  LT -> x : merge xs b
  EQ -> x : merge xs ys
  GT -> y : merge a ys

binarySearch :: (Vector.Unbox a, Ord a) => a -> Vector a -> Int
binarySearch x v = go 0 (Vector.length v - 1)
  where
    go !l !r
      | l >= r = l
      | otherwise =
          let !m = (l + r) `shiftR` 1
           in case compare x (Vector.unsafeIndex v m) of
                LT -> go l m
                EQ -> m
                GT -> go (m + 1) r
