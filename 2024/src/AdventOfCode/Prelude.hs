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
    Pair (..),
    Triple (..),
    Vec2 (Vec2),
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
    (&&&),
    findFirst,
    sortBy,
    sortOn,
    sepBy',
    sepBy1',
    sepEndBy',
    sepEndBy1',
    count,
    for,
  )
where

import AdventOfCode.Position (Direction (..), Position (..))
import AdventOfCode.Vec2 (Vec2 (Vec2))
import Control.Applicative.Combinators hiding
  ( count,
    endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Applicative.Combinators.NonEmpty
import Control.Arrow ((&&&))
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

data Solution
  = forall a.
  Solution
  { parser :: Parser a,
    solver :: a -> (Int, Int)
  }

sepBy1' :: Parser a -> Parser b -> Parser (NonEmpty a)
sepBy1' p sep = do
  !x <- p
  xs <- (sep *> sepBy' p sep) <|> pure []
  pure (x :| xs)

sepEndBy' :: Parser a -> Parser b -> Parser [a]
sepEndBy' p sep = sepBy' p sep <* optional sep

sepEndBy1' :: Parser a -> Parser b -> Parser (NonEmpty a)
sepEndBy1' p sep = do
  !x <- p
  xs <- (sep *> sepEndBy' p sep) <|> pure []
  pure (x :| xs)

count :: (Foldable f) => (a -> Bool) -> f a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst f = go
  where
    go = \case
      [] -> Nothing
      x : xs -> f x <|> go xs

for :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
for !start !stop m = go start
  where
    go !i
      | i > stop = pure ()
      | otherwise = m i >> go (i + 1)

-- | Strict Pair
data Pair a b = Pair !a !b
  deriving (Show, Eq, Ord)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair a1 b1 <> Pair a2 b2 = Pair (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend = (<>)
  mconcat = foldl' (flip mappend) mempty

-- | Strict Triple
data Triple a b c = Triple !a !b !c
  deriving (Show, Eq, Ord)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Triple a b c)
  where
  Triple a1 b1 c1 <> Triple a2 b2 c2 =
    Triple (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Triple a b c) where
  mempty = Triple mempty mempty mempty
  mappend = (<>)
  mconcat = foldl' (flip mappend) mempty
