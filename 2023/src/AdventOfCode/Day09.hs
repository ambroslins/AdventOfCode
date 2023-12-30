module AdventOfCode.Day09 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseLine :: Parser (Vector Int)
parseLine =
  Vector.fromList <$> Parser.signed Parser.decimal `sepBy` Parser.char ' '

diff :: Vector Int -> Vector Int
diff xs = Vector.zipWith (-) (Vector.drop 1 xs) xs

solve1 :: [Vector Int] -> Int
solve1 = sum . map extrapolate
  where
    extrapolate :: Vector Int -> Int
    extrapolate xs
      | Vector.all (== 0) xs = 0
      | otherwise = Vector.last xs + extrapolate (diff xs)

solve2 :: [Vector Int] -> Int
solve2 = sum . map extrapolate
  where
    extrapolate :: Vector Int -> Int
    extrapolate xs
      | Vector.all (== 0) xs = 0
      | otherwise = Vector.head xs - extrapolate (diff xs)
