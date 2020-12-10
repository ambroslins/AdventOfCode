module AdventOfCode.Day03 where

import AdventOfCode.Prelude

data Square = Open | Tree
  deriving (Eq)

parse :: String -> [[Square]]
parse = map (map f) . lines
  where
    f '.' = Open
    f '#' = Tree

solution :: Solution
solution = simpleSolution parse solve1 solve2

solve1 :: [[Square]] -> Int
solve1 grid = length $ filter (== Tree) $ slope 3 1 (map cycle grid)

solve2 :: [[Square]] -> Int
solve2 grid =
  product $
    map (\(r, d) -> length $ filter (== Tree) $ slope r d grid') [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    grid' = map cycle grid

slope :: Int -> Int -> [[Square]] -> [Square]
slope _ _ [] = []
slope r d (x : xs) = head x : slope r d xs'
  where
    xs' = drop (d -1) $ map (drop r) xs
