module AdventOfCode.PQueue
  ( PQueue,
    empty,
    null,
    singleton,
    fromList,
    insert,
    deleteMin,
  )
where

import Data.Bifunctor (second)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Prelude hiding (null)

newtype PQueue a = PQueue (IntMap [a])

empty :: PQueue a
empty = PQueue IntMap.empty

null :: PQueue a -> Bool
null (PQueue m) = IntMap.null m

singleton :: Int -> a -> PQueue a
singleton k v = PQueue (IntMap.singleton k (pure v))

fromList :: [(Int, a)] -> PQueue a
fromList =
  PQueue . IntMap.fromListWith (<>) . map (second pure)
{-# INLINE fromList #-}

insert :: Int -> a -> PQueue a -> PQueue a
insert k v (PQueue m) =
  PQueue (IntMap.alter (Just . maybe (pure v) (v :)) k m)
{-# INLINE insert #-}

deleteMin :: PQueue a -> Maybe (Int, a, PQueue a)
deleteMin (PQueue m) = case IntMap.minViewWithKey m of
  Nothing -> Nothing
  Just ((k, vs), m') -> case vs of
    [v] -> Just (k, v, PQueue m')
    v : xs -> Just (k, v, PQueue (IntMap.insert k xs m'))
    [] -> error "PQueue.deleteMin: empty list"
{-# INLINE deleteMin #-}
