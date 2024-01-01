module AdventOfCode.Search
  ( dfs,
    dfsOn,
    dfsN,
    dfsOnN,
    bfs,
    bfsOn,
    bfsN,
    bfsOnN,
    dijkstraOnInt,
  )
where

import AdventOfCode.BucketQueue qualified as BQ
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.HashMap.Internal qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IntSet qualified as IntSet
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

dijkstraOnInt ::
  (a -> Int) ->
  (Int -> a -> [(Int, a)]) ->
  [(Int, a)] ->
  [(Int, a)]
dijkstraOnInt rep children roots = runST $ do
  bq <- BQ.fromList 1024 roots
  let go !seen =
        BQ.dequeue bq >>= \case
          Nothing -> pure []
          Just (p, x)
            | r `IntSet.member` seen -> go seen
            | otherwise -> do
                BQ.enqueueList bq (children p x)
                ((p, x) :) <$> go (IntSet.insert r seen)
            where
              r = rep x
  go IntSet.empty
{-# INLINE dijkstraOnInt #-}

unsafeInsert :: (Hashable a) => a -> HashSet a -> HashSet a
unsafeInsert x = HashSet.fromMap . HashMap.unsafeInsert x () . HashSet.toMap
