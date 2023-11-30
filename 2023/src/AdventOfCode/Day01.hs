module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude

solution :: Solution
solution =
  Solution
    { parser = (sum <$> (Parser.decimal @Int `sepEndBy1` Parser.endOfLine)) `sepBy1` Parser.endOfLine,
      part1 = maximum,
      part2 = sum . take 3 . sortBy (flip compare)
    }
