module AdventOfCode.Prelude
  ( Solution (..),
    ByteString,
    HashMap,
    HashSet,
    IntMap,
    Map,
    Parser,
    Set,
    Vector,
    Generic,
    NFData,
    module Control.Applicative.Combinators,
    module Data.Maybe,
    module Data.Either,
    sortBy,
    sortOn,
    foldl',
    foldMap',
  )
where

import Control.Applicative.Combinators
import Control.DeepSeq (NFData)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable (foldMap')
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.List (foldl', sortBy, sortOn)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Solution = forall a b c.
  (Show b, Show c, NFData a, NFData b, NFData c) =>
  Solution
  { parser :: Parser a,
    part1 :: a -> b,
    part2 :: a -> c
  }
