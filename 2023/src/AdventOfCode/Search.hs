module AdventOfCode.Search
  ( dfs,
    dfsOn,
    dfsN,
    dfsOnN,
    bfs,
    bfsOn,
    bfsN,
    bfsOnN,
    bfsOnInt,
    dijkstraOnInt,
  )
where

import AdventOfCode.BucketQueue qualified as BucketQueue
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.HashMap.Internal qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntSet qualified as IntSet
import Data.Vector.Unboxed (Unbox)
import Deque.Lazy qualified as Deque
import GHC.IsList (fromList)

dfs :: (Hashable a) => (a -> [a]) -> a -> [a]
dfs = dfsOn id
{-# INLINE dfs #-}

dfsN :: (Hashable a) => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id
{-# INLINE dfsN #-}

dfsOn :: (Hashable r) => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn hash children root = dfsOnN hash children [root]
{-# INLINE dfsOn #-}

dfsOnN :: (Hashable r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
dfsOnN hash children = go HashSet.empty
  where
    go !seen = \case
      [] -> []
      (x : xs)
        | h `HashSet.member` seen -> go seen xs
        | otherwise -> x : go (unsafeInsert h seen) (children x <> xs)
        where
          h = hash x
{-# INLINE dfsOnN #-}

bfs :: (Hashable a) => (a -> [a]) -> a -> [a]
bfs = bfsOn id
{-# INLINE bfs #-}

bfsN :: (Hashable a) => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id
{-# INLINE bfsN #-}

bfsOn :: (Hashable r) => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn hash children root = bfsOnN hash children [root]
{-# INLINE bfsOn #-}

bfsOnN :: (Hashable r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOnN hash children = go HashSet.empty . fromList
  where
    go !seen queue = case Deque.uncons queue of
      Nothing -> []
      Just (x, xs)
        | h `HashSet.member` seen -> go seen xs
        | otherwise -> x : go (unsafeInsert h seen) (xs <> fromList (children x))
        where
          h = hash x

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

unsafeInsert :: (Hashable a) => a -> HashSet a -> HashSet a
unsafeInsert x = HashSet.fromMap . HashMap.unsafeInsert x () . HashSet.toMap
