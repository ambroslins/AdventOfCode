module AdventOfCode.Day20 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser ByteString
parseLine = Parser.line

solve :: [ByteString] -> (Int, Int)
solve ls = (length ls, BS.length $ head ls)
