module AdventOfCode.Day07 where

import AdventOfCode.Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set

data Bag = Bag String String
  deriving (Eq, Ord, Show)

data Rule = Rule Bag (Map Bag Int)
  deriving (Eq, Ord, Show)

solution :: Solution
solution = Solution (parseRule `sepEndBy1` eol) solve1 solve2

solve1 :: [Rule] -> Int
solve1 input = length $ filter containsShinyGoldBag $ Map.keys ruleMap
  where
    ruleMap :: Map.Map Bag (Set.Set Bag)
    ruleMap = Map.fromList $ map (\(Rule b bs) -> (b, Map.keysSet bs)) input
    containsShinyGoldBag b =
      maybe
        False
        (\s -> Set.member (Bag "shiny" "gold") s || any containsShinyGoldBag s)
        (Map.lookup b ruleMap)

solve2 :: [Rule] -> Int
solve2 input = subtract 1 $ bagSize (Bag "shiny" "gold")
  where
    ruleMap :: Map.Map Bag (Map.Map Bag Int)
    ruleMap = Map.fromList $ map (\(Rule b bs) -> (b, bs)) input
    bagSize b =
      maybe
        1
        ((+ 1) . sum . map (\(b', n) -> n * bagSize b') . Map.toList)
        $ Map.lookup b ruleMap

parseBag :: Parser Bag
parseBag = Bag <$> word <*> word <* (try (string "bags") <|> string "bag")

word :: Parser String
word = lexeme (some letterChar)

parseRule :: Parser Rule
parseRule = do
  b <- parseBag
  space >> string "contain" >> space
  let pair = do
        n <- decimal
        space
        b' <- parseBag
        pure (b', n)
  bs <- (pair `sepBy1` (char ',' >> hspace)) <|> ([] <$ string "no other bags")
  char '.'
  pure $ Rule b (Map.fromList bs)
