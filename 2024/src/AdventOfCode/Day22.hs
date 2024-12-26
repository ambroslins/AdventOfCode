module AdventOfCode.Day22 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (foldM, when)
import Control.Monad.ST.Strict (ST, runST)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.Int (Int32)
import Data.List qualified as List
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word (Word16, Word32, Word64, Word8)

solution :: Solution
solution =
  Solution
    { parser = Parser.decimal `sepEndBy` Parser.endOfLine,
      solver = solve
    }

solve :: [Word32] -> (Word64, Word16)
solve numbers = (part1, VU.maximum part2)
  where
    Pair part1 part2 =
      List.foldl1' go $
        parMap rseq solveChunk $
          chunks 255 numbers
    go (Pair acc1 acc2) (Pair p1 p2) =
      Pair (acc1 + p1) (VU.zipWith (+) acc2 p2)

solveChunk :: [Word32] -> Pair Word64 (VU.Vector Word16)
solveChunk numbers = runST $ do
  bananas <- MVU.replicate 130321 0
  seen <- MVU.replicate 130321 0
  part1 <-
    foldM (\acc (i, n) -> (+ acc) <$> solveSecret i bananas seen n) 0 $
      zip [1 ..] numbers
  part2 <- VU.unsafeFreeze bananas
  pure $ Pair part1 part2

solveSecret ::
  Word8 ->
  MVU.MVector s Word16 ->
  MVU.MVector s Word8 ->
  Word32 ->
  ST s Word64
solveSecret i bananas seen n0 = do
  let n1 = nextSecret n0
      n2 = nextSecret n1
      n3 = nextSecret n2
      p0 = n0 `rem` 10
      p1 = n1 `rem` 10
      p2 = n2 `rem` 10
      p3 = n3 `rem` 10
  let go !j !number !p !a !b !c
        | j <= 0 = pure $ fromIntegral number
        | otherwise = do
            let !next = nextSecret number
                !price = next `rem` 10
                !d = 9 + price - p
                !index = fromIntegral $ 6589 * a + 361 * b + 19 * c + d
            s <- MVU.read seen index
            when (s /= i) $ do
              MVU.write seen index i
              MVU.modify bananas (+ fromIntegral price) index
            go (j - 1) next price b c d

  go (2000 - 3 :: Int32) n3 p3 (9 + p1 - p0) (9 + p2 - p1) (9 + p3 - p2)

nextSecret :: Word32 -> Word32
nextSecret !n =
  let n1 = ((n `shiftL` 6) `xor` n) .&. mask
      n2 = ((n1 `shiftR` 5) `xor` n1) .&. mask
      n3 = ((n2 `shiftL` 11) `xor` n2) .&. mask
   in n3

mask :: Word32
mask = 16777215

chunks :: Int -> [a] -> [[a]]
chunks n = go
  where
    go xs
      | null xs = []
      | otherwise = let (c, ys) = List.splitAt n xs in c : go ys
