module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS

solution :: Solution
solution =
  Solution
    { parser = Parser.line `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

solve1 = length

solve2 = maximum . map BS.length
