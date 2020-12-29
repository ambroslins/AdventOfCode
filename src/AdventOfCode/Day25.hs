module AdventOfCode.Day25 where

import AdventOfCode.Prelude
import Data.List

solution :: Solution
solution = simpleSolution parseInput solve1 (const (0 :: Int))

parseInput :: String -> (Int, Int)
parseInput s = (x, y)
  where
    x : y : _ = map read $ lines s

solve1 :: (Int, Int) -> Int
solve1 (pubCard, pubDoor) = (!! loopSizeCard) $ iterate (transform pubDoor) 1
  where
    transform subjectNumber value = value * subjectNumber `rem` 20201227
    Just loopSizeCard = elemIndex pubCard $ iterate (transform 7) 1