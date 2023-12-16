module AdventOfCode.Search where

import AdventOfCode.Prelude
import Data.HashMap.Internal qualified as HashMap
import Data.HashSet qualified as HashSet

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

unsafeInsert :: (Hashable a) => a -> HashSet a -> HashSet a
unsafeInsert x = HashSet.fromMap . HashMap.unsafeInsert x () . HashSet.toMap
