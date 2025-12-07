module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet

solution :: Solution
solution =
  Solution
    { parser = do
        l : ls <- Parser.lines
        let start = fromMaybe (error "no start") $ BS.elemIndex (c2w 'S') l
        pure (start, filter (not . null) $ map (BS.elemIndices (c2w '^')) ls),
      solver = uncurry solve
    }

solve :: Int -> [[Int]] -> (Int, Int)
solve start ls = second sum $ foldl' go (0, IntMap.singleton start 1) splitters
  where
    splitters = map IntSet.fromDistinctAscList ls
    go (!acc, !beams) splitter =
      let hits = IntMap.restrictKeys beams splitter
          splits =
            IntMap.unionWith
              (+)
              (IntMap.mapKeysMonotonic (subtract 1) hits)
              (IntMap.mapKeysMonotonic (+ 1) hits)
       in ( acc + IntMap.size hits,
            IntMap.unionWith (+) (IntMap.difference beams hits) splits
          )
