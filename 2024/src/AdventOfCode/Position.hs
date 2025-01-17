{-# LANGUAGE TypeFamilies #-}

module AdventOfCode.Position
  ( Direction (..),
    Position (..),
    move,
    moveN,
    turnRight,
    turnLeft,
    invert,
    origin,
    toTuple,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Vector.Generic qualified as Generic
import Data.Vector.Generic.Mutable qualified as MGeneric
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed.Mutable (MVector)
import Prelude hiding (map)

data Direction = North | East | South | West
  deriving (Eq, Show, Enum)

instance Hashable Direction where
  hashWithSalt salt = hashWithSalt salt . fromEnum

instance NFData Direction where
  rnf d = d `seq` ()

data Position = Position {row :: !Int, col :: !Int}
  deriving (Eq, Ord, Show)

instance Semigroup Position where
  Position row1 col1 <> Position row2 col2 = Position (row1 + row2) (col1 + col2)

instance Hashable Position where
  hashWithSalt salt Position {row, col} = hashWithSalt salt (row, col)

instance NFData Position where
  rnf p = p `seq` ()

move :: Direction -> Position -> Position
move dir = moveN dir 1
{-# INLINE move #-}

moveN :: Direction -> Int -> Position -> Position
moveN dir n =
  case dir of
    North -> \Position {row, col} -> Position {row = row - n, col}
    East -> \Position {row, col} -> Position {row, col = col + n}
    South -> \Position {row, col} -> Position {row = row + n, col}
    West -> \Position {row, col} -> Position {row, col = col - n}
{-# INLINE moveN #-}

turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North
{-# INLINE turnRight #-}

turnLeft :: Direction -> Direction
turnLeft = \case
  North -> West
  East -> North
  South -> East
  West -> South
{-# INLINE turnLeft #-}

invert :: Direction -> Direction
invert = \case
  North -> South
  East -> West
  South -> North
  West -> East
{-# INLINE invert #-}

origin :: Position
origin = Position {row = 0, col = 0}
{-# INLINE origin #-}

toTuple :: Position -> (Int, Int)
toTuple Position {row, col} = (row, col)

newtype instance MVector s Position = MV_Position (MVector s (Int, Int))

newtype instance Vector Position = V_Position (Vector (Int, Int))

instance MGeneric.MVector MVector Position where
  basicLength (MV_Position v) = MGeneric.basicLength v
  basicUnsafeSlice i n (MV_Position v) = MV_Position $ MGeneric.basicUnsafeSlice i n v
  basicOverlaps (MV_Position v1) (MV_Position v2) = MGeneric.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Position <$> MGeneric.basicUnsafeNew n
  basicInitialize (MV_Position v) = MGeneric.basicInitialize v
  basicUnsafeReplicate n x = MV_Position <$> MGeneric.basicUnsafeReplicate n (toTuple x)
  basicUnsafeRead (MV_Position v) i = uncurry Position <$> MGeneric.basicUnsafeRead v i
  basicUnsafeWrite (MV_Position v) i x = MGeneric.basicUnsafeWrite v i (toTuple x)
  basicClear (MV_Position v) = MGeneric.basicClear v
  basicSet (MV_Position v) x = MGeneric.basicSet v (toTuple x)
  basicUnsafeCopy (MV_Position v1) (MV_Position v2) = MGeneric.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Position v1) (MV_Position v2) = MGeneric.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Position v) n = MV_Position <$> MGeneric.basicUnsafeGrow v n

instance Generic.Vector Vector Position where
  basicUnsafeFreeze (MV_Position v) = V_Position <$> Generic.basicUnsafeFreeze v
  basicUnsafeThaw (V_Position v) = MV_Position <$> Generic.basicUnsafeThaw v
  basicLength (V_Position v) = Generic.basicLength v
  basicUnsafeSlice i n (V_Position v) = V_Position $ Generic.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Position v) i = uncurry Position <$> Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Position mv) (V_Position v) = Generic.basicUnsafeCopy mv v
  elemseq _ = seq

instance Unbox Position
