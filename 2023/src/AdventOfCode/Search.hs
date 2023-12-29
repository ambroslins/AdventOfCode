module AdventOfCode.Search where

import AdventOfCode.Prelude
import Data.HashMap.Internal qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as PQueue
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

dijkstra :: (Hashable a) => (Int -> a -> [(Int, a)]) -> a -> [(Int, a)]
dijkstra children root = dijkstraOnN id children [(0, root)]
{-# INLINE dijkstra #-}

dijkstraOnN :: (Hashable r, Ord p) => (a -> r) -> (p -> a -> [(p, a)]) -> [(p, a)] -> [(p, a)]
dijkstraOnN hash children = go HashSet.empty . PQueue.fromList
  where
    insertPq pq (p, x) = PQueue.insert p x pq
    go !seen = \case
      PQueue.Empty -> []
      (p, x) :< pq
        | h `HashSet.member` seen -> go seen pq
        | otherwise ->
            (p, x) : go (unsafeInsert h seen) (foldl' insertPq pq (children p x))
        where
          h = hash x
{-# INLINE dijkstraOnN #-}

unsafeInsert :: (Hashable a) => a -> HashSet a -> HashSet a
unsafeInsert x = HashSet.fromMap . HashMap.unsafeInsert x () . HashSet.toMap
