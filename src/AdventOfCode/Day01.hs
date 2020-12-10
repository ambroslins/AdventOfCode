module AdventOfCode.Day01  where

import AdventOfCode.Prelude

solution :: Solution
solution = simpleSolution (map read . lines) solve1 solve2

solve1 :: [Int] -> Int
solve1 xs = fromMaybe 0 $ asum $ map f xs
  where
    f :: Int -> Maybe Int
    f x = (* x) <$> find ((== 2020) . (+ x)) xs

solve2 :: [Int] -> Int
solve2 = maybe 0 (\(a, b, c) -> a * b * c) . find (\(a, b, c) -> a + b + c == 2020) . triples
  where
    pairs [] = []
    pairs (x : xs) = map (x,) xs ++ pairs xs
    triples [] = []
    triples (x : xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs
