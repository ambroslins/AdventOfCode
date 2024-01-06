module AdventOfCode.Day09 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = coerce . foldMap' extrapolate
    }

parseLine :: Parser (Vector Int)
parseLine =
  Vector.fromList <$> Parser.signed Parser.decimal `sepBy` Parser.char ' '

extrapolate :: Vector Int -> (Sum Int, Sum Int)
extrapolate xs =
  coerce $
    foldr
      (\d (x, y) -> (x + Vector.last d, Vector.head d - y))
      (0, 0)
      diffs
  where
    diffs = takeWhile (not . Vector.all (== 0)) $ iterate diff xs

diff :: Vector Int -> Vector Int
diff xs = Vector.zipWith (-) (Vector.drop 1 xs) xs
