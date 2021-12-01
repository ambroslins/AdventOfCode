module Data.Matrix where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Matrix a = Matrix Int (Vector a)
  deriving (Eq, Ord)

instance Show a => Show (Matrix a) where
  show = show . toLists

instance Functor Matrix where
  fmap f (Matrix n v) = Matrix n $ fmap f v

toVectors :: Matrix a -> Vector (Vector a)
toVectors (Matrix n v) = Vector.fromList $ go v
  where
    go v' =
      let (x, xs) = Vector.splitAt n v'
       in if Vector.null xs then [x] else x : go xs

toLists :: Matrix a -> [[a]]
toLists = map Vector.toList . Vector.toList . toVectors

fromVectors :: Vector (Vector a) -> Maybe (Matrix a)
fromVectors vs =
  if all ((== n) . Vector.length) vs
    then Just $ Matrix n $ Vector.concat $ Vector.toList vs
    else Nothing
  where
    n = maybe 0 Vector.length $ vs Vector.!? 0

fromLists :: [[a]] -> Maybe (Matrix a)
fromLists xss =
  if all ((== n) . length) xss
    then Just $ Matrix n $ Vector.fromList $ concat xss
    else Nothing
  where
    n = case xss of
      [] -> 0
      xs : _ -> length xs

size :: Matrix a -> (Int, Int)
size (Matrix n v) = (Vector.length v `div` n, n)

sizeM :: Matrix a -> Int
sizeM = fst . size

sizeN :: Matrix a -> Int
sizeN = snd . size

generate :: Int -> Int -> (Int -> Int -> a) -> Matrix a
generate m n f = Matrix n $ Vector.generate (n * m) (\i -> uncurry (flip f) (i `quotRem` n))

column :: Matrix a -> Int -> Maybe (Vector a)
column mat@(Matrix n v) c = Vector.sequence $ Vector.generate (sizeM mat) (\i -> v Vector.!? (i * n + c))

row :: Matrix a -> Int -> Maybe (Vector a)
row mat@(Matrix n v) r =
  if 0 <= r && r < sizeM mat
    then Just $ Vector.slice (n * r) n v
    else Nothing

index :: Matrix a -> Int -> Int -> Maybe a
index (Matrix n v) x y = if 0 <= x && x < n then v Vector.!? (x + y * n) else Nothing

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(!?) mat = uncurry (index mat)

unsafeIndex :: Matrix a -> Int -> Int -> a
unsafeIndex (Matrix n v) x y = v Vector.! (x + y * n)

(!) :: Matrix a -> (Int, Int) -> a
(!) mat = uncurry (unsafeIndex mat)

sequence :: Monad m => Matrix (m a) -> m (Matrix a)
sequence (Matrix n v) = Matrix n <$> Vector.sequence v

imap :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
imap f (Matrix n v) = Matrix n $ Vector.imap (\i -> uncurry f (i `quotRem` n)) v

transpose :: Matrix a -> Matrix a
transpose mat = uncurry (flip generate) (size mat) $ flip (unsafeIndex mat)

joinVertical :: Matrix a -> Matrix a -> Matrix a
joinVertical (Matrix n v) (Matrix n' v')
  | n == n' = Matrix n $ v Vector.++ v'
  | otherwise = error "different n"

joinHorizontal :: Matrix a -> Matrix a -> Matrix a
joinHorizontal m m' = transpose $ joinVertical (transpose m) (transpose m')
