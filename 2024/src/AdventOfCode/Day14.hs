module AdventOfCode.Day14 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS

solution :: Solution
solution =
  Solution
    { parser = Parser.line `sepEndBy` Parser.endOfLine,
      solver = length &&& (maximum . map BS.length)
    }
