module AdventOfCode.Day05 where

import AdventOfCode.Prelude
import qualified Data.Set as Set

solution :: Solution
solution = simpleSolution lines solve1 solve2

solve1 :: [String] -> Int
solve1 = maximum . map bin

solve2 :: [String] -> Int
solve2 input =
  maybe 0 (+ 1) $
    find (\x -> (x + 2) `Set.member` seats && (x + 1) `Set.notMember` seats) seats
  where
    seats = Set.fromList $ map bin input

bin :: String -> Int
bin xs =
  sum $
    zipWith
      (\x y -> if x `elem` ['B', 'R'] then y else 0)
      (reverse xs)
      $ iterate (* 2) 1
