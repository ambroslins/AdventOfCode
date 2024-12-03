module AdventOfCode.Day03 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude

data Instruction = Mul !Int !Int | Do | Don't
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = Parser.many1' (parseLoop) <* Parser.takeByteString,
      solver = solve
    }

parseMul :: Parser Instruction
parseMul = do
  _ <- Parser.string "mul("
  lhs <- Parser.decimal
  _ <- Parser.string ","
  rhs <- Parser.decimal
  _ <- Parser.string ")"
  pure $ Mul lhs rhs

parseInstruction :: Parser Instruction
parseInstruction =
  Parser.choice
    [ parseMul,
      Do <$ Parser.string "do()",
      Don't <$ Parser.string "don't()"
    ]

parseLoop :: Parser Instruction
parseLoop = parseInstruction <|> (skipNext *> parseLoop)
  where
    skipNext = Parser.anyChar *> Parser.skipWhile (\c -> c /= 'm' && c /= 'd')

solve :: [Instruction] -> (Int, Int)
solve is = (p1, p2)
  where
    (_, !p1, !p2) = foldl' go (True, 0, 0) is
    go (!enabled, !acc1, !acc2) = \case
      Mul lhs rhs
        | enabled -> (enabled, acc1 + prod, acc2 + prod)
        | otherwise -> (enabled, acc1 + prod, acc2)
        where
          prod = lhs * rhs
      Do -> (True, acc1, acc2)
      Don't -> (False, acc1, acc2)
