module AdventOfCode.Day06 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (dropSpace)
import Data.List qualified as List
import Data.Word (Word8)

solution :: Solution
solution =
  Solution
    { parser = fromMaybe (error "impossible") . List.unsnoc <$> Parser.lines,
      solver = uncurry solve
    }

solve :: [ByteString] -> ByteString -> (Int, Int)
solve nums ops = let (_, !p1, !p2) = BS.foldl' go (0, 0, 0) ops in (p1, p2)
  where
    go (!i, !acc1, !acc2) c
      | c == c2w ' ' = (i + 1, acc1, acc2)
      | c == c2w '+' = (i + 1, part1 (+) acc1 i, part2 (+) acc2 i)
      | otherwise = (i + 1, acc1 + part1 (*) 1 i, acc2 + part2 (*) 1 i)
    n = BS.length ops - 1
    part1 :: (Int -> Int -> Int) -> Int -> Int -> Int
    part1 op unit i =
      foldl'
        (\acc -> op acc . BS.foldl' accDecimal 0 . BS.takeWhile Parser.isDigit8 . dropSpace . BS.drop i)
        unit
        nums
    part2 :: (Int -> Int -> Int) -> Int -> Int -> Int
    part2 op unit i =
      foldl' op unit $
        takeWhile (> 0) $
          [foldl' (\acc l -> accDecimal acc $ BS.index l j) 0 nums | j <- [i .. n]]

    accDecimal :: Int -> Word8 -> Int
    accDecimal !acc !c
      | c == c2w ' ' = acc
      | otherwise = 10 * acc + digit c

    digit :: Word8 -> Int
    digit c = fromIntegral (c - c2w '0')
