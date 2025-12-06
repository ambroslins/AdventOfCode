module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser Int
parseLine = do
  dir <- Parser.anyChar
  distance <- Parser.decimal
  pure $! if dir == 'L' then negate distance else distance

solve :: [Int] -> (Int, Int)
solve rs = let (_, !p1, !p2) = foldl' go (50, 0, 0) rs in (p1, p2)
  where
    go (!dial, !p1, !p2) rot =
      let (!d, !m) = (dial + rot) `divMod` 100
          !isZero = if m == 0 then 1 else 0
       in ( m,
            p1 + isZero,
            if rot >= 0 then p2 + d else p2 - d - fromEnum (dial == 0) + isZero
          )
