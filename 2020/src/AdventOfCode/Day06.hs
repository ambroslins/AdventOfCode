module AdventOfCode.Day06 where

import AdventOfCode.Prelude
import qualified Data.Set as Set

type Group = [Set Char]

parseGroup :: Parser Group
parseGroup = (Set.fromList <$> some letterChar) `sepEndBy1` eol

solution :: Solution
solution = Solution (parseGroup `sepBy` eol) solve1 solve2

solve1 :: [Group] -> Int
solve1 = sum . map (Set.size . foldr1 Set.union)

solve2 :: [Group] -> Int
solve2 = sum . map (Set.size . foldr1 Set.intersection)
