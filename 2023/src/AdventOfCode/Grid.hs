module AdventOfCode.Grid
  ( Grid,
    Direction (..),
    Position (..),
    parse,
    nrow,
    ncol,
    sliceRow,
    sliceCol,
    box,
    inside,
    index,
    unsafeIndex,
    findPosition,
  )
where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import Data.Vector qualified as Boxed
import Data.Vector.Generic (Vector)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Prelude hiding (map)

data Grid v a = Grid
  { ncol :: !Int,
    nrow :: !Int,
    cells :: !(v a)
  }
  deriving (Functor)

parse :: Parser (Grid Unboxed.Vector Char)
parse = do
  r :| rs <- Parser.line `sepEndBy1'` Parser.endOfLine
  let ncol = BS.length r
      cells = Unboxed.concat $ List.map bsToVector (r : rs)
      (nrow, rest) = Unboxed.length cells `divMod` ncol
  when (rest /= 0) $ fail "Grid is not rectangular"
  pure $ Grid {ncol, nrow, cells}

bsToVector :: BS.ByteString -> Unboxed.Vector Char
bsToVector bs = Unboxed.generate (BS.length bs) (BS.index bs)

box :: (Vector Unboxed.Vector a) => Grid Unboxed.Vector a -> Grid Boxed.Vector a
box grid = grid {cells = Vector.convert $ cells grid}

sliceRow :: (Vector v a) => Int -> Grid v a -> Maybe (v a)
sliceRow i Grid {nrow, ncol, cells}
  | i >= 0 && i < nrow = Just $ Vector.slice (i * ncol) ncol cells
  | otherwise = Nothing

sliceCol :: (Vector v a) => Int -> Grid v a -> Maybe (v a)
sliceCol i Grid {nrow, ncol, cells}
  | i >= 0 && i < ncol = Just $ Vector.generate nrow (\r -> cells Vector.! (r * ncol + i))
  | otherwise = Nothing

inside :: Position -> Grid v a -> Bool
inside Position {row, col} Grid {nrow, ncol} =
  row >= 0 && row < nrow && col >= 0 && col < ncol

index :: (Vector v a) => Grid v a -> Position -> Maybe a
index grid position
  | inside position grid = Just $ unsafeIndex grid position
  | otherwise = Nothing

unsafeIndex :: (Vector v a) => Grid v a -> Position -> a
unsafeIndex Grid {ncol, cells} Position {row, col} =
  cells Vector.! (row * ncol + col)

findPosition :: (Vector v a) => (a -> Bool) -> Grid v a -> Maybe Position
findPosition p Grid {ncol, cells} =
  case Vector.findIndex p cells of
    Nothing -> Nothing
    Just i ->
      let (row, col) = i `divMod` ncol
       in Just $! Position {row, col}
