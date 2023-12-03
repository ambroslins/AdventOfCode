module AdventOfCode.Prelude
  ( Solution (..),
    ByteString,
    Generic,
    HashMap,
    HashSet,
    IntMap,
    Map,
    NFData,
    Parser,
    Set,
    Vector,
    module Control.Applicative.Combinators,
    module Control.Applicative.Combinators.NonEmpty,
    module Control.Monad,
    module Data.Bifunctor,
    module Data.Either,
    module Data.Function,
    module Data.Functor,
    module Data.Foldable,
    module Data.Foldable1,
    module Data.Maybe,
    module Data.List.NonEmpty,
    sortBy,
    sortOn,
  )
where

import Control.Applicative.Combinators hiding (endBy1, sepBy1, sepEndBy1, some, someTill)
import Control.Applicative.Combinators.NonEmpty
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Attoparsec.ByteString (Parser)
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable (foldMap', foldl')
import Data.Foldable1 (Foldable1, foldMap1', foldl1', foldr1)
import Data.Function (fix, on, (&))
import Data.Functor (($>), (<$), (<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
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
