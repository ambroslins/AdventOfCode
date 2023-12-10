module AdventOfCode.Grid
  ( Grid,
    Direction (..),
    Position (..),
    parse,
    rows,
    cols,
    inside,
    index,
    unsafeIndex,
    findPosition,
    move,
    turnRight,
    turnLeft,
  )
where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.Hashable (Hashable (hashWithSalt))

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

data Grid a = Grid
  { cols :: !Int,
    rows :: !Int,
    cells :: !ByteString,
    cell :: Char -> a
  }
  deriving (Functor)

parse :: Parser (Grid Char)
parse = do
  cells <- Parser.takeByteString
  let rows = BS.count '\n' cells
      (cols, rest) = BS.length cells `divMod` rows
  when (rest /= 0) $ fail "Grid is not rectangular"
  pure $ Grid {cols = cols - 1, rows, cells, cell = id}

inside :: Position -> Grid a -> Bool
inside Position {row, col} Grid {rows, cols} =
  row >= 0 && row < rows && col >= 0 && col < cols

index :: Grid a -> Position -> Maybe a
index grid position
  | inside position grid = Just $ unsafeIndex grid position
  | otherwise = Nothing

unsafeIndex :: Grid a -> Position -> a
unsafeIndex Grid {cols, cells, cell} Position {row, col} =
  cell $ BS.index cells (row * (cols + 1) + col) -- +1 for the newline

findPosition :: (a -> Bool) -> Grid a -> Maybe Position
findPosition p Grid {cols, cell, cells} =
  case BS.findIndex p' cells of
    Nothing -> Nothing
    Just i ->
      let (row, col) = i `divMod` (cols + 1)
       in Just Position {row, col}
  where
    p' c = (c /= '\n') && p (cell c)

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
