module AdventOfCode.Day19 where

import AdventOfCode.Prelude
import Data.Foldable (foldlM)
import qualified Data.Vector as Vector

data Rule
  = Char Char
  | SubRule [[Int]]
  deriving (Eq, Show)

parseRule :: Parser (Int, Rule)
parseRule = do
  key <- decimal
  char ':' >> space
  rule <-
    (Char <$> between (char '"') (char '"') letterChar)
      <|> SubRule <$> (decimal `sepEndBy` char ' ') `sepBy` string "| "
  pure (key, rule)

parseInput :: Parser (Vector Rule, [String])
parseInput = do
  rules <- parseRule `sepEndBy` eol
  space
  messages <- some letterChar `sepEndBy` eol
  pure (Vector.replicate (1 + maximum (map fst rules)) undefined Vector.// rules, messages)

solution :: Solution
solution = Solution parseInput (uncurry solve1) (uncurry solve2)

solve1 :: Vector Rule -> [String] -> Int
solve1 rules = length . filter (validate rules 0)

solve2 :: Vector Rule -> [String] -> Int
solve2 = solve1 . (Vector.// [(8, SubRule [[42], [42, 8]]), (11, SubRule [[42, 31], [42, 11, 31]])])

validate :: Vector Rule -> Int -> String -> Bool
validate rules index message = elem "" $ go message index
  where
    go :: String -> Int -> [String]
    go msg i = case rules Vector.! i of
      Char c -> case msg of
        x : xs | x == c -> [xs]
        _ -> []
      SubRule rs -> concatMap (foldlM go msg) rs
