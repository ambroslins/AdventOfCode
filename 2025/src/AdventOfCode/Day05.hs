module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntMap.Strict qualified as IntMap

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
solve ranges ids = (count inRange ids, IntMap.foldlWithKey' go 0 rangeMap)
  where
    rangeMap = mergeRanges ranges
    inRange !i = case IntMap.lookupLE i rangeMap of
      Nothing -> False
      Just (_, end) -> i <= end
    go !acc !start !end = acc + end - start + 1

mergeRanges :: [(Int, Int)] -> IntMap Int
mergeRanges = foldl' go IntMap.empty
  where
    go !m (!start, !end) =
      let left = IntMap.lookupLE start m
          right = IntMap.lookupGT start m
          s = case left of
            Just (sl, el) | start <= el -> sl -- overlap the left range
            _ -> start
          e = case right of
            Just (sr, er) | end >= sr -> max end er -- overlap the right range
            _ -> maybe end (max end . snd) left
       in IntMap.insert s e $ case right of
            Just (sr, _) | end >= sr -> IntMap.delete sr m
            _ -> m
