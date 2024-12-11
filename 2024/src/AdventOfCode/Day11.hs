module AdventOfCode.Day11 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString qualified as BS
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Debug.Trace (traceShow)

solution :: Solution
solution =
  Solution
    { parser = (Parser.decimal `sepBy'` Parser.whitespace) <* Parser.endOfLine,
      solver = solve 25 &&& solve 75
    }

{-
solve' :: Int -> [Int] -> Int
solve' blinks stones = runST $ do
  cache <- MVector.replicate (blinks * cacheSize) (0 :: Int)

  let cacheInsert !b !n !s =
        when (n < cacheSize) $ MVector.write cache (n * blinks + b) s
      cacheLookup !b !n
        | n < cacheSize = MVector.read cache (n * blinks + b)
        | otherwise = pure 0

      blink !acc !b !n
        | b < 0 = pure (acc + 1)
        | otherwise = do
            cached <- cacheLookup b n
            if cached > 0
              then pure cached
              else do
                s <- case n of
                  0 -> blink acc (b - 1) 1
                  _
                    | Just (q, r) <- splitDigits n -> do
                        acc' <- blink acc (b - 1) q
                        blink acc' (b - 1) r
                    | otherwise -> blink acc (b - 1) (n * 2024)
                cacheInsert b n s
                pure s

  sum <$> mapM (blink 0 blinks) stones
  where
    cacheSize = 1000
-}

solve :: Int -> [Int] -> Int
solve n = sum . parMap rseq (blink n)

blink :: Int -> Int -> Int
blink = go 0
  where
    go !acc !n !i
      | n == 0 = acc + 1
      | i == 0 = go acc (n - 1) 1
      | Just (q, r) <- splitDigits i =
          go (go acc (n - 1) q) (n - 1) r
      | otherwise = go acc (n - 1) (i * 2024)

splitDigits :: Int -> Maybe (Int, Int)
splitDigits n = go 10 10
  where
    go !b1 !b2
      | n < b2 = Nothing
      | n < 10 * b2 = Just $! n `quotRem` b1
      | otherwise = go (b1 * 10) (b2 * 100)
