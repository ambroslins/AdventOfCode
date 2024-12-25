module AdventOfCode.Day25 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List qualified as List
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)

data Heights = Heights !Word8 !Word8 !Word8 !Word8 !Word8

type Key = Heights

type Lock = Heights

solution :: Solution
solution =
  Solution
    { parser = Grid.parse `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& const 0
    }

fit :: Key -> Lock -> Bool
fit (Heights k1 k2 k3 k4 k5) (Heights l1 l2 l3 l4 l5) =
  k1 >= l1 && k2 >= l2 && k3 >= l3 && k4 >= l4 && k5 >= l5

solve1 :: [Grid VU.Vector Char] -> Int
solve1 schematics = sum $ map (\k -> count (fit k) lockHeights) keyHeights
  where
    (locks, keys) =
      List.partition
        (VU.all (== '#') . fromJust . Grid.row 0)
        schematics
    toHeights c g = case map (fromIntegral . fromJust . VU.elemIndex c) (Grid.cols g) of
      [h1, h2, h3, h4, h5] -> Heights h1 h2 h3 h4 h5
      hs -> error $ "solve1: unexpected heights: " <> show hs
    lockHeights = map (toHeights '.') locks
    keyHeights = map (toHeights '#') keys
