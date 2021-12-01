module AdventOfCode.Day10 where

import AdventOfCode.Prelude
import Data.List (sort)

parse :: String -> [Int]
parse = map read . lines

solution :: Solution
solution = simpleSolution parse solve1 solve2

solve1 :: [Int] -> Int
solve1 = uncurry (*) . go (1, 1) . sort
  where
    go (a, b) (x : y : xs)
      | x + 1 == y = go (a + 1, b) (y : xs)
      | x + 3 == y = go (a, b + 1) (y : xs)
      | otherwise = go (a, b) (y : xs)
    go x _ = x

solve2 :: [Int] -> Integer
solve2 = f 0 . go . sort
  where
    f y = sum . map snd . takeWhile ((<= y + 3) . fst)
    go [] = []
    go [x] = [(x, 1)]
    go (x : xs) = (x, f x ys) : ys
      where
        ys = go xs