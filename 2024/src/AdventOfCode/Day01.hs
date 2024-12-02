module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.IntMap.Strict qualified as IntMap
import Data.Monoid (Sum (..))

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser (Int, Int)
parseLine = do
  !l <- Parser.decimal
  Parser.whitespace
  !r <- Parser.decimal
  pure (l, r)

solve :: [(Int, Int)] -> (Int, Int)
solve lists =
  ( sum $ zipWith (\l r -> abs (l - r)) (unfoldMap ls) (unfoldMap rs),
    getSum $
      IntMap.foldMapWithKey
        (\l n -> Sum (n * l * (IntMap.findWithDefault 0 l rs)))
        ls
  )
  where
    (ls, rs) = foldl' insert mempty lists
    insert (!ml, !mr) (!l, !r) =
      (IntMap.insertWith (+) l 1 ml, IntMap.insertWith (+) r 1 mr)
    unfoldMap = IntMap.foldrWithKey (\k n ns -> replicate n k <> ns) []
