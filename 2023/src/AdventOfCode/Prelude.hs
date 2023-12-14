module AdventOfCode.Prelude
  ( Solution (..),
    ByteString,
    Generic,
    HashMap,
    HashSet,
    Hashable,
    IntMap,
    Map,
    NFData,
    Parser,
    Set,
    Position (..),
    Direction (..),
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
    sepBy',
    sepEndBy',
    sepEndBy1',
    count,
  )
where

import AdventOfCode.Position (Direction (..), Position (..))
import Control.Applicative.Combinators hiding
  ( count,
    endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Applicative.Combinators.NonEmpty
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Attoparsec.ByteString (Parser, sepBy')
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable (foldMap', foldl')
import Data.Foldable1 (Foldable1, foldMap1', foldl1', foldr1)
import Data.Function (fix, on, (&))
import Data.Functor (($>), (<$), (<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import GHC.Generics (Generic)

data Solution = forall a b c.
  (Show b, Show c, NFData b, NFData c) =>
  Solution
  { parser :: Parser a,
    part1 :: a -> b,
    part2 :: a -> c
  }

sepEndBy' :: Parser a -> Parser b -> Parser [a]
sepEndBy' p sep = sepBy' p sep <* optional sep

sepEndBy1' :: Parser a -> Parser b -> Parser (NonEmpty a)
sepEndBy1' p sep = do
  !x <- p
  xs <- (sep *> sepEndBy' p sep) <|> pure []
  pure (x :| xs)

count :: (Foldable f) => (a -> Bool) -> f a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0
