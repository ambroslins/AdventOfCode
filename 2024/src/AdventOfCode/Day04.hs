module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Internal (c2w)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Monoid (Sum (..))
import Data.Word (Word8)

solution :: Solution
solution =
  Solution
    { parser = Parser.takeByteString,
      solver = solve
    }

solve :: ByteString -> (Int, Int)
solve input = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) =
      foldMap' go $
        BS.elemIndices 'A' $
          BS.take (BS.length input - 2 * start) $
            BS.drop start input
    go i = Pair (Sum $ countXmas i) (if isX i then 1 else 0)
    ncols =
      fromMaybe (error "expected '\\n' in input") $
        BS.elemIndex '\n' input

    start = ncols + 2 -- first row + newline + first col of second row
    deltas =
      [ Triple (-2 * d) (-d) d
      | dr <- [-ncols - 1, 0, ncols + 1], -- +1 for the \n
        dc <- [-1, 0, 1],
        let d = dr + dc,
        d /= 0
      ]

    countXmas i =
      let !j = i + start
       in count
            ( \(Triple dx dm ds) ->
                input BS.!? (j + dx) == Just 'X'
                  && unsafeIndex input (j + dm) == m
                  && unsafeIndex input (j + ds) == s
            )
            deltas

    isX i = diag (nw, se) && diag (ne, sw)
      where
        !j = i + start
        !nw = unsafeIndex input (j - ncols - 2)
        !ne = unsafeIndex input (j - ncols)
        !sw = unsafeIndex input (j + ncols)
        !se = unsafeIndex input (j + ncols + 2)

    diag p = p == (s, m) || p == (m, s)

m, s :: Word8
m = c2w 'M'
s = c2w 'S'
