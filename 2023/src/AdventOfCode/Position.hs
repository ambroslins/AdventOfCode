module AdventOfCode.Position
  ( Direction (..),
    Position (..),
    move,
    turnRight,
    turnLeft,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (hashWithSalt))
import Prelude hiding (map)

data Direction = North | East | South | West
  deriving (Eq, Show)

instance NFData Direction where
  rnf d = d `seq` ()

data Position = Position {row :: !Int, col :: !Int}
  deriving (Eq, Show)

instance Hashable Position where
  hashWithSalt salt Position {row, col} = hashWithSalt salt (row, col)

instance NFData Position where
  rnf p = p `seq` ()

move :: Direction -> Position -> Position
move dir Position {row, col} =
  case dir of
    North -> Position {row = row - 1, col}
    East -> Position {row, col = col + 1}
    South -> Position {row = row + 1, col}
    West -> Position {row, col = col - 1}

turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

turnLeft :: Direction -> Direction
turnLeft = \case
  North -> West
  East -> North
  South -> East
  West -> South
