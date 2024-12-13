module AdventOfCode.Day13 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Monoid (Sum (..))

data Machine
  = Machine
  { buttonA :: !(Vec2 Int),
    buttonB :: !(Vec2 Int),
    prize :: !(Vec2 Int)
  }
  deriving (Show)

solution :: Solution
solution =
  Solution
    { parser = parseMachine `sepBy` Parser.endOfLine,
      solver = solve
    }

parseButton :: ByteString -> Parser (Vec2 Int)
parseButton label = do
  Parser.symbol "Button "
  Parser.symbol label
  Parser.symbol ": X+"
  x <- Parser.decimal
  Parser.symbol ", Y+"
  y <- Parser.decimal
  pure $ Vec2 x y

parsePrize :: Parser (Vec2 Int)
parsePrize = do
  Parser.symbol "Prize: X="
  x <- Parser.decimal
  Parser.symbol ", Y="
  y <- Parser.decimal
  pure $ Vec2 x y

parseMachine :: Parser Machine
parseMachine = do
  buttonA <- parseButton "A" <* Parser.endOfLine
  buttonB <- parseButton "B" <* Parser.endOfLine
  prize <- parsePrize <* Parser.endOfLine
  pure $ Machine {buttonA, buttonB, prize}

solve :: [Machine] -> (Int, Int)
solve machines = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) = foldMap' go machines
    go m =
      Pair
        (Sum $ tokens m)
        (Sum $ tokens m {prize = prize m + pure 10000000000000})

tokens :: Machine -> Int
tokens Machine {buttonA = Vec2 xa ya, buttonB = Vec2 xb yb, prize = Vec2 x y}
  | denominator /= 0 && restB == 0 && restA == 0 = 3 * a + b
  | otherwise = 0
  where
    numerator = ya * x - xa * y
    denominator = ya * xb - xa * yb
    (b, restB) = numerator `quotRem` denominator
    (a, restA) = (y - yb * b) `quotRem` ya
