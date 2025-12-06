module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List

solution :: Solution
solution =
  Solution
    { parser = do
        ranges <- parseRange `sepEndBy'` Parser.endOfLine
        Parser.endOfLine
        ids <- Parser.decimal `sepEndBy'` Parser.endOfLine
        pure (ranges, ids),
      solver = uncurry solve
    }

parseRange :: Parser (Int, Int)
parseRange = do
  !start <- Parser.decimal
  _ <- Parser.char '-'
  !end <- Parser.decimal
  pure (start, end)

solve :: [(Int, Int)] -> [Int] -> (Int, Int)
solve ranges ids = (count inRange ids, IntMap.foldlWithKey' sumSize 0 rangeMap)
  where
    rangeMap = mergeRanges ranges
    inRange !i = case IntMap.lookupLE i rangeMap of
      Nothing -> False
      Just (_, end) -> i <= end
    sumSize !acc !start !end = acc + end - start + 1

mergeRanges :: [(Int, Int)] -> IntMap Int
mergeRanges = merge IntMap.empty . List.sort
  where
    merge !m [] = m
    merge !m [(start, end)] = IntMap.insert start end m
    merge !m ((s1, e1) : (s2, e2) : rest)
      | e1 >= s2 = merge m ((s1, max e1 e2) : rest)
      | otherwise = merge (IntMap.insert s1 e1 m) $ (s2, e2) : rest
