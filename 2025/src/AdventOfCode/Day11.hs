module AdventOfCode.Day11 (solution) where

import AdventOfCode.Graph qualified as Graph
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Control.Monad.State.Strict (MonadState (..), evalState, execState)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.Char (isLower)
import Data.Foldable (Foldable (toList))
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Debug.Trace (traceShow, traceShowId)

solution :: Solution
solution =
  Solution
    { parser = parseEdge `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseEdge :: Parser (Int, [Int])
parseEdge = do
  from <- Parser.takeWhile1 isLower
  _ <- Parser.symbol ":"
  tos <- Parser.takeWhile1 isLower `sepBy1'` Parser.whitespace
  pure (perfectHash from, map perfectHash $ toList tos)

solve :: [(Int, [Int])] -> (Int, Int)
solve edges =
  ( go you out,
    if fftToDac > 0
      then go svr fft * fftToDac * go dac out
      else go svr dac * go dac fft * go fft out
  )
  where
    graph = Graph.fromIntMap $ IntMap.fromList $ (out, []) : edges
    go = countPaths graph
    fftToDac = go fft dac

countPaths :: Graph.Graph -> Int -> Int -> Int
countPaths graph from to = runST $ do
  cache <- MVector.replicate (Vector.length $ Graph.nodes graph) (-1)
  let go !node
        | node == to = pure 1
        | otherwise = do
            c <- MVector.read cache node
            if c >= 0
              then pure c
              else do
                let (_, neighbours) = Vector.unzip $ Graph.neighbours graph node
                paths <- Vector.foldM' (\acc n -> (+ acc) <$> go n) 0 neighbours
                MVector.write cache node paths
                pure paths
  go from

-- Because our Graph implementation works with indices, not node values,
-- we map the known nodes to the first indices.
-- Otherwise we would have to find the index of each known node after
-- building the graph.
perfectHash :: ByteString -> Int
perfectHash = \case
  "you" -> 0
  "out" -> 1
  "svr" -> 2
  "dac" -> 3
  "fft" -> 4
  bs -> BS.foldl' (\acc c -> acc * 26 + fromIntegral (c - c2w 'a')) 5 bs

out, you, svr, dac, fft :: Int
out = perfectHash "out"
you = perfectHash "you"
svr = perfectHash "svr"
dac = perfectHash "dac"
fft = perfectHash "fft"
