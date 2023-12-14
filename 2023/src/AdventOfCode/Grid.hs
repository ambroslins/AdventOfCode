module AdventOfCode.Grid
  ( Grid,
    Direction (..),
    Position (..),
    parse,
    fromRows,
    fromCols,
    transpose,
    nrows,
    ncols,
    cells,
    row,
    col,
    rows,
    cols,
    box,
    inside,
    index,
    unsafeIndex,
    findPosition,
    findPositions,
  )
where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (col, row)
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import Data.Vector qualified as Boxed
import Data.Vector.Generic (Vector)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed

data Grid v a = Grid
  { ncols :: !Int,
    nrows :: !Int,
    cells :: !(v a)
  }
  deriving (Functor)

parse :: Parser (Grid Unboxed.Vector Char)
parse = do
  r :| rs <- Parser.line `sepEndBy1'` Parser.endOfLine
  let ncols = BS.length r
      cells = Unboxed.concat $ List.map byteStringToVector (r : rs)
      (nrows, rest) = Unboxed.length cells `divMod` ncols
  when (rest /= 0) $ fail "Grid is not rectangular"
  pure $ Grid {ncols, nrows, cells}

fromRows :: (Vector v a) => [v a] -> Grid v a
fromRows = \case
  [] -> Grid {ncols = 0, nrows = 0, cells = Vector.empty}
  (r : rs)
    | rest == 0 -> Grid {ncols, nrows, cells}
    | otherwise -> error "Grid is not rectangular"
    where
      ncols = Vector.length r
      (nrows, rest) = Vector.length cells `divMod` ncols
      cells = Vector.concat (r : rs)
{-# INLINE fromRows #-}

fromCols :: (Vector v a) => [v a] -> Grid v a
fromCols = transpose . fromRows
{-# INLINE fromCols #-}

byteStringToVector :: BS.ByteString -> Unboxed.Vector Char
byteStringToVector bs = Unboxed.generate (BS.length bs) (BS.index bs)

box :: (Vector Unboxed.Vector a) => Grid Unboxed.Vector a -> Grid Boxed.Vector a
box grid = grid {cells = Vector.convert $ cells grid}

row :: (Vector v a) => Int -> Grid v a -> Maybe (v a)
row i grid
  | i >= 0 && i < nrows grid = Just $ unsafeRow i grid
  | otherwise = Nothing
{-# INLINE row #-}

unsafeRow :: (Vector v a) => Int -> Grid v a -> v a
unsafeRow i Grid {ncols, cells} =
  Vector.slice (i * ncols) ncols cells
{-# INLINE unsafeRow #-}

rows :: (Vector v a) => Grid v a -> [v a]
rows grid = List.map (`unsafeRow` grid) [0 .. nrows grid - 1]
{-# INLINE rows #-}

col :: (Vector v a) => Int -> Grid v a -> Maybe (v a)
col i grid
  | i >= 0 && i < ncols grid = Just $ unsafeCol i grid
  | otherwise = Nothing
{-# INLINE col #-}

unsafeCol :: (Vector v a) => Int -> Grid v a -> v a
unsafeCol i Grid {nrows, ncols, cells} =
  Vector.generate nrows (\r -> cells Vector.! (r * ncols + i))
{-# INLINE unsafeCol #-}

cols :: (Vector v a) => Grid v a -> [v a]
cols grid = List.map (`unsafeCol` grid) [0 .. ncols grid - 1]
{-# INLINE cols #-}

transpose :: (Vector v a) => Grid v a -> Grid v a
transpose Grid {ncols, nrows, cells} =
  Grid
    { ncols = nrows,
      nrows = ncols,
      cells = Vector.generate (ncols * nrows) f
    }
  where
    f i = let (r, c) = i `divMod` ncols in cells Vector.! (c * nrows + r)
{-# INLINE transpose #-}

inside :: Position -> Grid v a -> Bool
inside (Position r c) Grid {nrows, ncols} =
  r >= 0 && r < nrows && c >= 0 && c < ncols

index :: (Vector v a) => Grid v a -> Position -> Maybe a
index grid position
  | inside position grid = Just $ unsafeIndex grid position
  | otherwise = Nothing

unsafeIndex :: (Vector v a) => Grid v a -> Position -> a
unsafeIndex Grid {ncols, cells} (Position r c) =
  cells Vector.! (r * ncols + c)

findPosition :: (Vector v a) => (a -> Bool) -> Grid v a -> Maybe Position
findPosition p Grid {ncols, cells} =
  case Vector.findIndex p cells of
    Nothing -> Nothing
    Just i ->
      let (r, c) = i `divMod` ncols
       in Just $! Position r c

findPositions ::
  (Vector v a, Vector v Int) =>
  (a -> Bool) ->
  Grid v a ->
  Unboxed.Vector Position
findPositions p Grid {ncols, cells} =
  Vector.map (\i -> let (r, c) = i `divMod` ncols in Position r c) $
    Vector.convert $
      Vector.findIndices p cells
{-# INLINE findPositions #-}
