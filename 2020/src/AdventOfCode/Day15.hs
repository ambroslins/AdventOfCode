module AdventOfCode.Day15 where

import AdventOfCode.Prelude
import qualified Data.IntMap as IntMap

solution :: Solution
solution = Solution (decimal `sepBy` char ',') solve1 solve2

data Memory = Memory
  { lastNumber :: Int,
    turn :: Int,
    spoken :: IntMap Int
  }
  deriving (Eq, Show)

initMemory :: [Int] -> Memory
initMemory xs = Memory (last xs) (length xs + 1) (IntMap.fromList $ zip (init xs) [1 ..])

playTurn :: Memory -> Memory
playTurn (Memory l t s) = x `seq` s' `seq` t' `seq` Memory x t' s'
  where
    t' = t + 1
    s' = IntMap.insert l (t - 1) s
    x = maybe 0 ((t - 1) -) $ IntMap.lookup l s

solve1 :: [Int] -> Int
solve1 = lastNumber . until ((> 2020) . turn) playTurn . initMemory

solve2 :: [Int] -> Int
solve2 = lastNumber . until ((> 30000000) . turn) playTurn . initMemory
