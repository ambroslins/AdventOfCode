module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Sum (..))

data Equation = Equation {test :: !Int, numbers :: [Int]}

solution :: Solution
solution =
  Solution
    { parser = parseEquation `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseEquation :: Parser Equation
parseEquation = do
  test <- Parser.decimal
  _ <- Parser.string ": "
  numbers <- NonEmpty.toList <$> (Parser.decimal `sepBy1'` Parser.char ' ')
  pure $ Equation {test, numbers}

solve :: [Equation] -> (Int, Int)
solve equations = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) = mconcat $ parMap rseq go equations
    go eq@Equation {test}
      | couldBeTrue1 eq = Pair (Sum test) (Sum test)
      | couldBeTrue2 eq = Pair 0 (Sum test)
      | otherwise = Pair 0 0

couldBeTrue1 :: Equation -> Bool
couldBeTrue1 Equation {test, numbers} = go 0 numbers
  where
    go !acc = \case
      [] -> acc == test
      n : ns
        | acc > test -> False
        | otherwise -> go (acc + n) ns || go (acc * n) ns

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
      | y < 10 = 10
      | y < 100 = 100
      | y < 1000 = 1000
      | otherwise = error "(|||): rhs too large"
