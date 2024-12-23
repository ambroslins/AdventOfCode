module AdventOfCode.Day22 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.IntMap qualified as IntMap
import Data.Vector.Unboxed qualified as VU

solution :: Solution
solution =
  Solution
    { parser = Parser.decimal `sepEndBy` Parser.endOfLine,
      solver = solve
    }

solve :: [Int] -> (Int, Int)
solve numbers =
  ( sum $ map VU.last secrets,
    maximum $
      IntMap.unionsWith (+) $
        parMap rseq sequenceReturns secrets
  )
  where
    secrets = parMap rseq (VU.iterateN 2001 nextSecret) numbers

sequenceReturns :: VU.Vector Int -> IntMap Int
sequenceReturns secrets =
  VU.ifoldr'
    (\i s acc -> IntMap.insert s (prices VU.! (i + 4)) acc)
    IntMap.empty
    sequences
  where
    prices = VU.map (`mod` 10) secrets
    changes = VU.zipWith (-) (VU.drop 1 prices) prices
    sequences =
      VU.zipWith4
        hashSequence
        changes
        (VU.drop 1 changes)
        (VU.drop 2 changes)
        (VU.drop 3 changes)

hashSequence :: Int -> Int -> Int -> Int -> Int
hashSequence c0 c1 c2 c3 = (c0 + 10) + (c1 + 10) * 20 + (c2 + 10) * 400 + (c3 + 10) * 8000

nextSecret :: Int -> Int
nextSecret !n =
  let n1 = ((n `shiftL` 6) `xor` n) .&. mask
      n2 = ((n1 `shiftR` 5) `xor` n1) .&. mask
      n3 = ((n2 `shiftL` 11) `xor` n2) .&. mask
   in n3

mask :: Int
mask = 16777215
