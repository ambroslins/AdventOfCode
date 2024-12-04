module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS

solution :: Solution
solution =
  Solution
    { parser = Parser.takeByteString,
      solver = solve
    }

solve :: ByteString -> (Int, Int)
solve input =
  ( sum $ map countPart1 indices,
    count part2 indices
  )
  where
    ncols =
      fromMaybe (error "solve: expected '\\n' in input") $
        BS.elemIndex '\n' input
    indices = BS.elemIndices 'A' input
    countPart1 i =
      count
        (== "XMAS")
        [ [c | n <- [-2, -1, 0, 1], Just c <- [input BS.!? (i + n * (dx + dy))]]
        | dx <- [-1, 0, 1],
          dy <- [-ncols - 1, 0, ncols + 1]
        ]
    part2 i =
      [ c
      | dx <- [-1, 1],
        dy <- [-ncols - 1, ncols + 1],
        Just c <- [input BS.!? (i + dx + dy)]
      ]
        `elem` ["MMSS", "SSMM", "MSMS", "SMSM"]
