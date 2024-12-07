module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.List.NonEmpty qualified as NonEmpty

data Equation = Equation {test :: !Int, numbers :: [Int]}

solution :: Solution
solution =
  Solution
    { parser = parseEquation `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseEquation :: Parser Equation
parseEquation = do
  test <- Parser.decimal
  _ <- Parser.string ": "
  numbers <- NonEmpty.toList <$> (Parser.decimal `sepBy1` Parser.char ' ')
  pure $ Equation {test, numbers}

solve :: [Equation] -> (Int, Int)
solve equations =
  ( sum $ map test $ filter couldBeTrue equations,
    sum $ map test $ filter couldBeTrue2 equations
  )

couldBeTrue :: Equation -> Bool
couldBeTrue Equation {test, numbers} = go 0 numbers
  where
    go !acc = \case
      [] -> acc == test
      n : ns -> go (acc + n) ns || go (acc * n) ns

couldBeTrue2 :: Equation -> Bool
couldBeTrue2 Equation {test, numbers} = go 0 numbers
  where
    go !acc = \case
      [] -> acc == test
      n : ns
        | acc > test -> False
        | otherwise -> go (acc + n) ns || go (acc * n) ns || go (acc ||| n) ns

(|||) :: Int -> Int -> Int
x ||| y = x * b + y
  where
    b
      | y >= 1000 = 10000
      | y >= 100 = 1000
      | y >= 10 = 100
      | otherwise = 10
