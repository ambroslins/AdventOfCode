module AdventOfCode.Grid
  ( Grid,
    Direction (..),
    Position (..),
    size,
    parse,
    map,
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
    create,
    newMutable,
    unsafeThaw,
    read,
    readMaybe,
    write,
    modify,
  )
where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (col, row)
import Control.Monad (when)
import Control.Monad.ST.Strict (ST, runST)
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import Data.Vector qualified as Boxed
import Data.Vector.Generic (Mutable, Vector)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Generic.Mutable (MVector)
import Data.Vector.Generic.Mutable qualified as MVector
import Data.Vector.Unboxed qualified as Unboxed
import Prelude hiding (map, read)

data Grid v a = Grid
  { ncols :: !Int,
    nrows :: !Int,
    cells :: !(v a)
  }
  deriving (Functor)

size :: Grid v a -> (Int, Int)
size Grid {nrows, ncols} = (nrows, ncols)

parse :: Parser (Grid Unboxed.Vector Char)
parse = do
  r :| rs <- Parser.line `sepEndBy1'` Parser.endOfLine
  let ncols = BS.length r
      cells = Unboxed.concat $ List.map byteStringToVector (r : rs)
      (nrows, rest) = Unboxed.length cells `divMod` ncols
  when (rest /= 0) $ fail "Grid is not rectangular"
  pure $ Grid {ncols, nrows, cells}

map :: (Vector v a, Vector v b) => (a -> b) -> Grid v a -> Grid v b
map f grid = grid {cells = Vector.map f (cells grid)}
{-# INLINE map #-}

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

create :: (Vector v a) => (forall s. ST s (Grid (Mutable v s) a)) -> Grid v a
create m = runST $ do
  grid <- m
  cs <- Vector.unsafeFreeze (cells grid)
  pure $ grid {cells = cs}
{-# INLINEABLE create #-}

newMutable :: (Vector v a) => Int -> Int -> a -> ST s (Grid (Mutable v s) a)
newMutable nrows ncols x = do
  cells <- MVector.replicate (nrows * ncols) x
  pure $ Grid {nrows, ncols, cells}
{-# INLINEABLE newMutable #-}

unsafeThaw :: (Vector v a) => Grid v a -> ST s (Grid (Mutable v s) a)
unsafeThaw grid = do
  mcells <- Vector.unsafeThaw (cells grid)
  pure $ grid {cells = mcells}
{-# INLINEABLE unsafeThaw #-}

read :: (MVector v a) => Grid (v s) a -> Position -> ST s a
read Grid {cells, ncols} (Position r c) =
  MVector.read cells (r * ncols + c)
{-# INLINEABLE read #-}

readMaybe :: (MVector v a) => Grid (v s) a -> Position -> ST s (Maybe a)
readMaybe grid pos
  | pos `inside` grid = Just <$> read grid pos
  | otherwise = pure Nothing
{-# INLINEABLE readMaybe #-}

write :: (MVector v a) => Grid (v s) a -> Position -> a -> ST s ()
write Grid {cells, ncols} (Position r c) x =
  MVector.write cells (r * ncols + c) x
{-# INLINEABLE write #-}

modify :: (MVector v a) => Grid (v s) a -> (a -> a) -> Position -> ST s ()
modify Grid {cells, ncols} f (Position r c) =
  MVector.modify cells f (r * ncols + c)
{-# INLINEABLE modify #-}
