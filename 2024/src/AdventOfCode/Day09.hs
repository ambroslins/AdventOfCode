{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}

module AdventOfCode.Day09 (solution) where

import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Monad.ST.Strict (ST, runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.STRef.Strict (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Debug.Trace (traceShow)
import Prelude hiding (id)

data FileNode = FileNode
  { id :: !Int,
    start :: !Int,
    size :: !Int
  }
  deriving (Show)

end :: FileNode -> Int
end FileNode {start, size} = start + size

{-
type NodeRef s a = STRef s (Node s a)

{-
data Node s a
    = Null
    | Node !(NodePtr s a) !a !(NodePtr s a)

insertAfter :: NodePtr s a -> a -> ST s (NodePtr s a)
insertAfter prev v =
  readSTRef prev >>= \case
    Null -> do
      next <- newSTRef Null
      newSTRef $ Node prev v next
    Node pp pv next -> do
      n <- newSTRef $ Node prev v next
      writeSTRef prev $ Node pp pv n
      readSTRef next >>= \case
        Null -> do
          nn <- newSTRef Null
          writeSTRef
-}

data Node s a = Node
  { prev :: !(NodeRef s a),
    value :: !a,
    next :: !(NodeRef s a)
  }

insertAfter :: NodeRef s a -> a -> ST s (NodeRef s a)
insertAfter prevRef v = do
  prevNode <- readSTRef prevRef
  nodeRef <- newSTRef $ Node {prev = prevRef, value = v, next = next prevNode}
  writeSTRef prevRef $ prevNode {next = nodeRef}
  modifySTRef' (next prevNode) $ \n -> n {prev = nodeRef}
  pure nodeRef
-}

{-

parseFiles :: Parser [File]
parseFiles = goFile 0 0
  where
    goFile id start = do
      size <- c2i <$> Parser.digit
      rest <- goFree (id + 1) (start + size) <|> pure []
      pure $ File {id, start, size} : rest
    goFree id start = do
      free <- c2i <$> Parser.digit
      goFile id (start + free)

solve :: [File] -> (Int, Int)
solve files = traceShow (compact2 files) (checksum $ compact1 files, checksum $ compact2 files)

compact1 :: [File] -> [File]
compact1 files = go 0 files (reverse files)
  where
    go _ [] _ = []
    go _ _ [] = []
    go !pos (x : xs) (y : ys)
      | id x >= id y = [y {start = pos}]
      | free <= 0 = x : go (end x) xs (y : ys)
      | free >= size y = y1 : go (end y1) (x : xs) ys
      | otherwise = y1 : x : go (end x) xs (y2 : ys)
      where
        free = start x - pos
        y1 = y {start = pos}
        y2 = y {size = size y - free}

compact2 :: [File] -> [File]
compact2 files = go 0 files (reverse files)
  where
    go _ [] _ = []
    go _ _ [] = []
    go !pos (x : xs) (y : ys)
      | id x >= id y = xs
      | free <= 0 = x : go (end x) xs (y : ys)
      | free >= size y = y1 : go (end y1) (filter ((/= id y) . id) $ x : xs) ys
      | otherwise = go (end x) (x : xs) ys
      where
        free = start x - pos
        y1 = y {start = pos}

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2
-}

solution :: Solution
solution =
  Solution
    { parser = parseFiles <$> (Parser.line <* Parser.endOfLine),
      solver = solve
    }

parseFiles :: ByteString -> Vector FileNode
parseFiles input =
  Vector.fromListN (BS.length input + 1 `div` 2) $
    go 0 0 $
      map c2i $
        BS.unpack input
  where
    go !id !pos (size : free : rest) =
      let file = FileNode {id, start = pos, size}
       in file : go (id + 1) (pos + size + free) rest
    go id !pos [size] = [FileNode {id, start = pos, size}]
    go _ _ [] = []

solve :: Vector FileNode -> (Int, Int)
solve files = (compact1 files, compact2 files)

compact1 :: Vector FileNode -> Int
compact1 files = runST $ do
  let total = end $ Vector.last files
  disk <- MVU.replicate total (-1)
  Vector.iforM_ files $ \i f -> do
    MVU.set (MVU.slice (start f) (size f) disk) i

  let nextFree !i = do
        b <- MVU.read disk i
        if b == -1 then pure i else nextFree (i + 1)
      prevFile !i
        | i < 0 = pure (-1)
        | otherwise = do
            b <- MVU.read disk i
            if b == -1 then prevFile (i - 1) else pure i
      compact !i !j = do
        i' <- nextFree i
        j' <- prevFile j
        if i >= j
          then pure ()
          else do
            MVU.swap disk i' j'
            compact (i' + 1) (j' - 1)

  compact 0 (MVU.length disk - 1)

  checksum <$> VU.unsafeFreeze disk

compact2 :: Vector FileNode -> Int
compact2 files = runST $ do
  let total = end $ Vector.last files
  disk <- MVU.replicate total (-1)
  Vector.iforM_ files $ \i f -> do
    MVU.set (MVU.slice (start f) (size f) disk) i

  let findIndex p !i
        | i >= MVU.length disk = pure (-1)
        | otherwise = do
            b <- MVU.read disk i
            if p b
              then pure i
              else findIndex p (i + 1)
      findBlock !size !i = do
        j <- findIndex (< 0) i
        if j == -1
          then pure (-1)
          else do
            k <- findIndex (>= 0) (j + 1)
            if (k - j) >= size || k == -1
              then pure j
              else findBlock size (k + 1)

  Vector.forM_ (Vector.reverse files) $ \f -> do
    j <- findBlock (size f) 0
    if j == -1 || j >= start f
      then pure ()
      else do
        MVU.set (MVU.slice j (size f) disk) (id f)
        MVU.set (MVU.slice (start f) (size f) disk) (-1)

  checksum <$> VU.unsafeFreeze disk

checksum :: VU.Vector Int -> Int
checksum disk = VU.ifoldl' go 0 disk
  where
    go acc i x
      | x >= 0 = acc + i * x
      | otherwise = acc

{-
compact1 :: Vector (Int, Int) -> [File]
compact1 files = filter (\f -> size f > 0) $ runST $ do
  fs <- Vector.thaw files

  let go !pos !i !j
        | i >= MVector.length fs = pure []
        {-
        \| i >= j = do
            (_, size) <- MVector.read fs i
            let f = File {id = i, start = pos, size}
            rest <- go (pos + size) (i + 1) j
            pure (f : rest)
            -}
        | otherwise = do
            (start1, size1) <- MVector.read fs i
            (start2, size2) <- MVector.read fs j
            let free = start1 - pos
            if
              | free == 0 -> do
                  let f = File {id = i, start = start1, size = size1}
                  rest <- go (end f) (i + 1) j
                  pure (f : rest)
              | free >= size2 -> do
                  MVector.write fs j (start2, 0)
                  let f = File {id = j, start = pos, size = size2}
                  rest <- go (end f) i (j - 1)
                  pure (f : rest)
              | otherwise -> do
                  MVector.write fs j (start2, size2 - free)
                  let f1 = File {id = i, start = start1, size = size1}
                  let f2 = File {id = j, start = pos, size = free}
                  rest <- go (end f1) (i + 1) j
                  pure (f2 : f1 : rest)

  go 0 0 (MVector.length fs - 1)

compact2 :: Vector (Int, Int) -> [File]
compact2 files = filter (\f -> size f > 0) $ runST $ do
  fs <- Vector.thaw files

  let go !pos !i !j
        | i >= MVector.length fs = pure []
        | i >= j = do
            (start, size) <- MVector.read fs i
            let f = File {id = i, start = start, size}
            rest <- go (pos + size) (i + 1) j
            pure (f : rest)
        | otherwise = do
            (start1, size1) <- MVector.read fs i
            (start2, size2) <- MVector.read fs j
            let free = start1 - pos
            if
              | free == 0 -> do
                  let f = File {id = i, start = start1, size = size1}
                  rest <- go (end f) (i + 1) j
                  pure (f : rest)
              | free >= size2 -> do
                  MVector.write fs j (start2, 0)
                  let f = File {id = j, start = pos, size = size2}
                  rest <- go (end f) i (j - 1)
                  pure (f : rest)
              | otherwise -> do
                  let f = File {id = i, start = start1, size = size1}
                  rest <- go (end f) (i + 1) (j - 1)
                  pure (f : rest)

  go 0 0 (MVector.length fs - 1)

checksum :: [File] -> Int
checksum = sum . map go
  where
    go f = sum [id f * i | i <- [start f .. end f - 1]]
    -}

{-
  where
    sizes = map c2i $ BS.unpack input
    total = sum sizes
    disk = Vector.create $ do
      d <- MVector.replicate total (-1 :: Int)
      traceShow total $ pure ()
      let write !i !pos = \case
            [] -> traceShow pos $ pure ()
            file : rest -> do
              let bs = MVector.slice pos file d
              MVector.set bs i
              case rest of
                [] -> traceShow pos $ pure ()
                free : rest' -> write (i + 1) (pos + file + free) rest'
      write 0 0 sizes
      let nextFree !i = do
            f <- MVector.read d i
            if f == -1 then pure i else nextFree (i + 1)
          prevFile !i
            | i < 0 = pure (-1)
            | otherwise = do
                f <- MVector.read d i
                if f == -1 then prevFile (i - 1) else pure i
          compact !i !j
            | i >= j = pure ()
            | otherwise = do
                MVector.swap d i j
                i' <- nextFree (i + 1)
                j' <- prevFile (j - 1)
                compact i' j'

      compact (head sizes) (total - 1)

      pure d

    checksum = Vector.ifoldl' (\acc i x -> acc + i * x) 0
-}
c2i :: Char -> Int
c2i c = Char.ord c - Char.ord '0'
