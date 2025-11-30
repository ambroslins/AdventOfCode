module AdventOfCode.Search
  ( dfsOnInt,
    bfsOnInt,
    dijkstraOnInt,
  )
where

import AdventOfCode.BucketQueue qualified as BucketQueue
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.IntSet qualified as IntSet
import Deque.Lazy qualified as Deque
import GHC.IsList (fromList)

dfsOnInt :: (a -> Int) -> (a -> [a]) -> [a] -> [a]
dfsOnInt rep children = go IntSet.empty
  where
    go !seen = \case
      [] -> []
      (x : xs)
        | r `IntSet.member` seen -> go seen xs
        | otherwise -> x : go (IntSet.insert r seen) (children x <> xs)
        where
          r = rep x
{-# INLINE dfsOnInt #-}

bfsOnInt :: (a -> Int) -> (a -> [a]) -> [a] -> [a]
bfsOnInt rep children = go IntSet.empty . fromList
  where
    go !seen queue = case Deque.uncons queue of
      Nothing -> []
      Just (x, xs)
        | r `IntSet.member` seen -> go seen xs
        | otherwise -> x : go (IntSet.insert r seen) (xs <> fromList (children x))
        where
          r = rep x
{-# INLINE bfsOnInt #-}

dijkstraOnInt ::
  (a -> Int) ->
  (Int -> a -> [(Int, a)]) ->
  [(Int, a)] ->
  [(Int, a)]
dijkstraOnInt rep children roots = runST $ do
  queue <- BucketQueue.fromList 256 roots
  flip fix IntSet.empty $ \go !seen ->
    BucketQueue.dequeue queue >>= \case
      Nothing -> pure []
      Just (p, x)
        | r `IntSet.member` seen -> go seen
        | otherwise -> do
            BucketQueue.enqueueList queue (children p x)
            ((p, x) :) <$> go (IntSet.insert r seen)
        where
          r = rep x
{-# INLINE dijkstraOnInt #-}
