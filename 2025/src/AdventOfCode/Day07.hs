module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (foldM)
import Control.Monad.ST.Strict (runST)
import Data.ByteString qualified as BS
import Data.Vector.Primitive.Mutable qualified as MVector

solution :: Solution
solution =
  Solution
    { parser = Parser.lines,
      solver = solve
    }

solve :: [ByteString] -> (Int, Int)
solve [] = error "no input"
solve (l : ls) = runST $ do
  timelines <- MVector.replicate width 0
  MVector.write timelines start 1
  let go !acc !i = do
        t <- MVector.read timelines i
        if t == 0
          then pure acc
          else do
            MVector.write timelines i 0
            MVector.modify timelines (+ t) (i - 1)
            MVector.modify timelines (+ t) (i + 1)
            pure $ acc + 1
  hits <- foldM go 0 splitters
  ts <- MVector.foldl' (+) 0 timelines
  pure (hits, ts)
  where
    width = BS.length l
    start = fromMaybe (error "no start") $ BS.elemIndex (c2w 'S') l
    splitters = concatMap (BS.elemIndices (c2w '^')) $ everyOther ls
    everyOther = \case
      _ : x : xs -> x : everyOther xs
      _ -> []
