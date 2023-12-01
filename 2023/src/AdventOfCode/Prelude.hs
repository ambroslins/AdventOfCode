module AdventOfCode.Prelude
  ( Solution (..),
    ByteString,
    HashMap,
    HashSet,
    IntMap,
    Map,
    Parser,
    Seq,
    Set,
    Vector,
    module Control.Applicative.Combinators,
    sortBy,
    sortOn,
  )
where

import Control.Applicative.Combinators
import Control.DeepSeq (NFData)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.List (sortBy, sortOn)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Vector (Vector)

data Solution = forall a b c.
  (Show b, Show c, NFData a, NFData b, NFData c) =>
  Solution
  { parser :: Parser a,
    part1 :: a -> b,
    part2 :: a -> c
  }
