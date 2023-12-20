module AdventOfCode.Day19 (solution) where

import AdventOfCode.Interval (Interval)
import AdventOfCode.Interval qualified as Interval
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.Identity (Identity (..))
import Data.Char (isAsciiLower)
import Data.Functor.Const (Const (..))
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HashMap

type Lens s a = forall f. (Functor f) => (a -> f a) -> s -> f s

get :: Lens s a -> s -> a
get l = getConst . l Const

set :: Lens s a -> a -> s -> s
set l f = runIdentity . l (Identity . const f)

data Part a = Part {x, m, a, s :: !a}
  deriving (Eq, Show)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens sa sas afa s = sas s <$> afa (sa s)

x', m', a', s' :: Lens (Part a) a
x' = lens x (\p x -> p {x})
m' = lens m (\p m -> p {m})
a' = lens a (\p a -> p {a})
s' = lens s (\p s -> p {s})

type Name = ByteString

data Operation = LessThan | GreaterThan
  deriving (Eq, Show)

data Result = Next !Name | Reject | Accept
  deriving (Eq, Show)

data Rule = Rule
  { field :: forall a. Lens (Part a) a,
    operation :: !Operation,
    value :: !Int,
    result :: !Result
  }

type Workflow = ([Rule], Result)

solution :: Solution
solution =
  Solution
    { parser = do
        ws <- parseWorkflow `sepEndBy` Parser.endOfLine
        Parser.endOfLine
        parts <- parsePart `sepEndBy'` Parser.endOfLine
        pure (HashMap.fromList ws, parts),
      part1 = uncurry solve1,
      part2 = uncurry solve2
    }

parseWorkflow :: Parser (Name, Workflow)
parseWorkflow = do
  name <- parseName
  workflow <- betweenCurly $ do
    rules <- parseRule `sepEndBy'` Parser.char ','
    end <- parseResult
    pure (rules, end)
  pure (name, workflow)

parseRule :: Parser Rule
parseRule = do
  fieldName <- Parser.satisfy isAsciiLower
  operation <- (LessThan <$ Parser.char '<') <|> (GreaterThan <$ Parser.char '>')
  value <- Parser.decimal <* Parser.char ':'
  result <- parseResult
  pure $
    Rule
      { operation,
        value,
        result,
        field = case fieldName of
          'x' -> x'
          'm' -> m'
          'a' -> a'
          's' -> s'
          _ -> error $ "bad field name: " <> show fieldName
      }

parseResult :: Parser Result
parseResult =
  Parser.choice
    [ Accept <$ Parser.char 'A',
      Reject <$ Parser.char 'R',
      Next <$> parseName
    ]

parsePart :: Parser (Part Int)
parsePart = betweenCurly $ do
  x <- "x=" *> Parser.decimal <* Parser.char ','
  m <- "m=" *> Parser.decimal <* Parser.char ','
  a <- "a=" *> Parser.decimal <* Parser.char ','
  s <- "s=" *> Parser.decimal
  pure $ Part {x, m, a, s}

parseName :: Parser Name
parseName = Parser.takeWhile isAsciiLower

betweenCurly :: Parser a -> Parser a
betweenCurly = between (Parser.char '{') (Parser.char '}')

solve1 :: HashMap Name Workflow -> [Part Int] -> Int
solve1 workflows = sum . map add . filter (accepted (workflows ! "in"))
  where
    add Part {x, m, a, s} = x + m + a + s
    accepted :: Workflow -> Part Int -> Bool
    accepted (rules, res) part = case rules of
      [] -> apply part res
      Rule {field, operation, value, result} : rs -> case operation of
        LessThan
          | get field part < value -> apply part result
          | otherwise -> accepted (rs, res) part
        GreaterThan
          | get field part > value -> apply part result
          | otherwise -> accepted (rs, res) part
    apply :: Part Int -> Result -> Bool
    apply part = \case
      Next name -> accepted (workflows ! name) part
      Reject -> False
      Accept -> True

solve2 :: HashMap Name Workflow -> [Part Int] -> Int
solve2 workflows _ = uncurry (go start) (workflows ! "in")
  where
    start = let i = Interval.make 1 4001 in Part {x = i, m = i, a = i, s = i}
    go :: Part (Interval Int) -> [Rule] -> Result -> Int
    go !part rules !res = case rules of
      [] -> apply part res
      Rule {field, operation, value, result} : rs
        | Interval.null interval -> 0
        | otherwise -> case operation of
            LessThan ->
              let (left, right) = Interval.split value interval
               in apply (set field left part) result
                    + go (set field right part) rs res
            GreaterThan ->
              let (left, right) = Interval.split (value + 1) (get field part)
               in apply (set field right part) result
                    + go (set field left part) rs res
        where
          interval = get field part
    apply :: Part (Interval Int) -> Result -> Int
    apply part@Part {x, m, a, s} = \case
      Next name -> uncurry (go part) (workflows ! name)
      Reject -> 0
      Accept -> Interval.size x * Interval.size m * Interval.size a * Interval.size s
