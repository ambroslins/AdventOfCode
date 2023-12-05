module AdventOfCode.Interval
  ( Interval,
    make,
    start,
    end,
    shift,
    intersection,
    difference,
    null,
  )
where

import Prelude hiding (null)

data Interval a = Interval {start :: !a, end :: !a}
  deriving (Eq, Show)

make :: (Ord a) => a -> a -> Interval a
make = Interval

shift :: (Num a) => a -> Interval a -> Interval a
shift x Interval {start, end} = Interval {start = start + x, end = end + x}

intersection :: (Ord a) => Interval a -> Interval a -> Interval a
intersection a b =
  Interval
    { start = max (start a) (start b),
      end = min (end a) (end b)
    }

difference :: (Ord a) => Interval a -> Interval a -> (Interval a, Interval a)
difference a b = (left, right)
  where
    left = Interval {start = min (start a) (start b), end = min (end a) (start b)}
    right = Interval {start = max (start a) (end b), end = max (end a) (end b)}

null :: (Ord a) => Interval a -> Bool
null i = start i >= end i
