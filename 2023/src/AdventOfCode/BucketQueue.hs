module AdventOfCode.BucketQueue
  ( BucketQueue,
    empty,
    null,
    fromList,
    enqueue,
    enqueueList,
    dequeue,
  )
where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MVector
import Prelude hiding (null)

newtype BucketQueue s a = BucketQueue (STRef s (BQ s a))

data BQ s a = BQ
  { firstNonEmpty :: !Int,
    buckets :: !(MVector s [a])
  }

empty :: Int -> ST s (BucketQueue s a)
empty n = do
  buckets <- MVector.replicate n []
  BucketQueue <$> newSTRef (BQ {firstNonEmpty = -1, buckets})

null :: BucketQueue s a -> ST s Bool
null (BucketQueue ref) = do
  BQ {firstNonEmpty} <- readSTRef ref
  pure $ firstNonEmpty < 0

enqueue :: BucketQueue s a -> Int -> a -> ST s ()
enqueue (BucketQueue ref) p x = do
  BQ {firstNonEmpty, buckets} <- readSTRef ref
  let d = p - MVector.length buckets
  bs <-
    if d < 0
      then pure buckets
      else do
        bs <- MVector.grow buckets (d + 1)
        traverse_ (flip (MVector.write bs) []) [MVector.length buckets .. p]
        pure bs
  MVector.modify bs (x :) p
  writeSTRef ref $
    if firstNonEmpty < 0
      then BQ {firstNonEmpty = p, buckets = bs}
      else BQ {firstNonEmpty = min firstNonEmpty p, buckets = bs}

dequeue :: BucketQueue s a -> ST s (Maybe (Int, a))
dequeue (BucketQueue ref) = do
  BQ {firstNonEmpty, buckets} <- readSTRef ref
  if firstNonEmpty < 0
    then pure Nothing
    else
      MVector.read buckets firstNonEmpty >>= \case
        [] -> error "BucketQueue.dequeue: empty bucket"
        [x] -> do
          MVector.write buckets firstNonEmpty []
          i <- findFirstNonEmpty (firstNonEmpty + 1) buckets
          writeSTRef ref $ BQ {firstNonEmpty = i, buckets}
          pure $ Just (firstNonEmpty, x)
        x : xs -> do
          MVector.write buckets firstNonEmpty xs
          pure $ Just (firstNonEmpty, x)

enqueueList :: BucketQueue s a -> [(Int, a)] -> ST s ()
enqueueList bq = traverse_ (uncurry (enqueue bq))

fromList :: Int -> [(Int, a)] -> ST s (BucketQueue s a)
fromList n xs = do
  q <- empty n
  traverse_ (uncurry (enqueue q)) xs
  pure q

findFirstNonEmpty :: Int -> MVector s [a] -> ST s Int
findFirstNonEmpty !i !v
  | i >= MVector.length v = pure (-1)
  | otherwise = do
      xs <- MVector.read v i
      if List.null xs then findFirstNonEmpty (i + 1) v else pure i
