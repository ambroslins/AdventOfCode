module AdventOfCode.Day09 where

import AdventOfCode.Prelude
import Data.List (tails)

parse :: String -> [Int]
parse = map read . lines

solution :: Solution
solution = simpleSolution (map read . lines) solve1 solve2

solve1 :: [Int] -> Int
solve1 = uncurry go . splitAt 25
  where
    go ys (x : xs) =
      if any (\(a, b) -> a /= b && a + b == x) $ pairs ys
        then go (tail ys ++ [x]) xs
        else x
    go _ _ = 0
    pairs [] = []
    pairs (x : xs) = map (x,) xs ++ pairs xs

solve2 :: [Int] -> Int
solve2 input = maybe 0 (\c -> maximum c + minimum c) cont
  where
    target = solve1 input
    go acc list
      | acc == target = Just []
      | otherwise = case list of
        (x : xs) | acc < target -> (x :) <$> go (acc + x) xs
        _ -> Nothing
    cont = asum $ map (go 0) $ tails input
