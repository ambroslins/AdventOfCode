module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseLine :: Parser (ByteString, [Int])
parseLine = do
  springs <- Parser.takeWhile1 (/= ' ') <* Parser.space
  groups <- Parser.decimal `sepEndBy'` Parser.char ','
  pure (springs, groups)

arrangements :: ByteString -> [Int] -> Int
arrangements = go 0 . BS.unpack
  where
    go :: Int -> [Char] -> [Int] -> Int
    go !c springs groups = case groups of
      []
        | '#' `elem` springs -> 0
        | otherwise -> 1
      (!g : gs) -> case springs of
        []
          | c == g && null gs -> 1
        '.' : rest
          | c == 0 -> go 0 rest groups
          | c == g -> go 0 rest gs
        '#' : rest
          | c < g -> go (c + 1) rest groups
        '?' : rest ->
          go c ('.' : rest) groups + go c ('#' : rest) groups
        _ -> 0

solve1 :: [(ByteString, [Int])] -> Int
solve1 = sum . map (uncurry arrangements)

solve2 :: [(ByteString, [Int])] -> Int
solve2 = sum . parMap rseq (uncurry arrangements . unfold)

unfold :: (ByteString, [Int]) -> (ByteString, [Int])
unfold (springs, groups) =
  ( BS.intercalate "?" (replicate 5 springs),
    concat (replicate 5 groups)
  )
