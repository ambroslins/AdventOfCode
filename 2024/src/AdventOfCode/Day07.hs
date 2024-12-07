module AdventOfCode.Day07 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Monoid (Sum (..))

data Equation = Equation {test :: !Int, numbers :: NonEmpty Int}

data Result = Equal1 | Equal2 | NotEqual
  deriving (Eq, Show)

instance Semigroup Result where
  Equal1 <> _ = Equal1
  Equal2 <> y = if y == NotEqual then Equal2 else y
  NotEqual <> y = y

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
  numbers <- Parser.decimal `sepBy1'` Parser.char ' '
  pure $ Equation {test, numbers}

solve :: [Equation] -> (Int, Int)
solve equations = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) = foldMap' go equations
    go eq@Equation {test} = case couldBeTrue eq of
      Equal1 -> Pair (Sum test) (Sum test)
      Equal2 -> Pair 0 (Sum test)
      NotEqual -> Pair 0 0

couldBeTrue :: Equation -> Result
couldBeTrue Equation {test, numbers = n :| ns} =
  go True test (reverse ns)
  where
    go !p1 !acc = \case
      []
        | acc /= n -> NotEqual
        | p1 -> Equal1
        | otherwise -> Equal2
      x : xs -> add <> mul <> cat
        where
          add = let d = acc - x in ensure (d >= 0) (go p1 d xs)
          mul =
            let (q, r) = acc `quotRem` x
             in ensure (r == 0) (go p1 q xs)
          cat =
            let (q, r) = acc `quotRem` base x
             in ensure (r == x) (go False q xs)

ensure :: Bool -> Result -> Result
ensure p r = if p then r else NotEqual

base :: Int -> Int
base n
  | n < 10 = 10
  | n < 100 = 100
  | n < 1000 = 1000
  | n < 10000 = 10000
  | otherwise = error "base: n too large"
