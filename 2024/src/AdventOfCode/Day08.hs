module AdventOfCode.Day08 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import AdventOfCode.Vec2 qualified as Vec2
import Control.Exception (assert)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List

solution :: Solution
solution =
  Solution
    { parser = Parser.takeByteString,
      solver = solve
    }

solve :: ByteString -> (Int, Int)
solve input = assert (rest == 0) (antinodes [1], antinodes [0 ..])
  where
    ncols =
      fromMaybe (error "solve: expected '\\n'") $
        BS.elemIndex '\n' input
    (nrows, rest) = BS.length input `quotRem` (ncols + 1)

    nodes =
      IntMap.fromListWith (<>)
        $ map
          ( \i ->
              let (y, x) = i `quotRem` (ncols + 1)
                  !freq = Char.ord $ BS.index input i
               in (freq, [Vec2 x y])
          )
        $ BS.findIndices (/= '.') input

    antinodes modes =
      IntSet.size $ IntSet.fromList $ map hash $ do
        (_freq, vs) <- IntMap.toList nodes
        !v1 : ts <- List.tails vs
        !v2 <- ts
        let !d = (v2 - v1)
            left = takeWhile inside $ map (\m -> v1 - Vec2.scale m d) modes
            right = takeWhile inside $ map (\m -> v2 + Vec2.scale m d) modes
        left ++ right

    inside (Vec2 x y) = 0 <= x && x < ncols && 0 <= y && y < nrows
    hash (Vec2 x y) = y * ncols + x
