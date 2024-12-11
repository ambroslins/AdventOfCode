{-# LANGUAGE MultiWayIf #-}

module AdventOfCode.Day11 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Data.Vector.Unboxed.Mutable qualified as MVector

solution :: Solution
solution =
  Solution
    { parser = (Parser.decimal `sepBy'` Parser.whitespace) <* Parser.endOfLine,
      solver = solve' 25 &&& solve' 75
    }

solve' :: Int -> [Int] -> Int
solve' blinks stones = runST $ do
  cache <- MVector.replicate (blinks * cacheSize) (0 :: Int)

  -- b : number of blinks left
  -- n : engraved number
  -- s : number of stones
  let cacheInsert !b !n !s =
        when (n < cacheSize) $ MVector.write cache (n * blinks + b) s
      cacheLookup !b !n
        | n < cacheSize = MVector.read cache (n * blinks + b)
        | otherwise = pure 0

      blink !b !n
        | b <= 0 = pure 1
        | otherwise = do
            cached <- cacheLookup b n
            if cached > 0
              then pure cached
              else do
                s <-
                  if
                    | n == 0 -> blink (b - 1) 1
                    | Just (l, r) <- splitDigits n -> do
                        sl <- blink (b - 1) l
                        sr <- blink (b - 1) r
                        pure (sl + sr)
                    | otherwise -> blink (b - 1) (n * 2024)
                cacheInsert b n s
                pure s

  sum <$> mapM (blink blinks) stones
  where
    cacheSize = 1000

splitDigits :: Int -> Maybe (Int, Int)
splitDigits !n = go 10 10
  where
    go !b1 !b2
      | n < b2 = Nothing
      | n < 10 * b2 = Just $! n `quotRem` b1
      | otherwise = go (b1 * 10) (b2 * 100)
