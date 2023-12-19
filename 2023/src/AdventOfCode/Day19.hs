module AdventOfCode.Day19 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Char (isAsciiLower)
import Data.HashMap.Strict qualified as HashMap

data Part = Part {x, m, a, s :: !Int}
  deriving (Eq, Show)

type Name = ByteString

data Destination = Next !Name | Reject | Accept

type Rule = Part -> Destination

solution :: Solution
solution =
  Solution
    { parser = do
        ws <- parseWorkflow `sepEndBy` Parser.endOfLine
        Parser.endOfLine
        parts <- parsePart `sepEndBy'` Parser.endOfLine
        pure (HashMap.fromList ws, parts),
      part1 = uncurry solve1,
      part2 = length . fst
    }

parseWorkflow :: Parser (Name, Rule)
parseWorkflow = do
  name <- parseName
  rule <- betweenCurly $ do
    rules <- parseRule `sepEndBy'` Parser.char ','
    dest <- parseDestination
    pure $ foldr ($) (const dest) rules
  pure (name, rule)

parseRule :: Parser (Rule -> Rule)
parseRule = do
  field <-
    Parser.choice
      [ x <$ Parser.char 'x',
        m <$ Parser.char 'm',
        a <$ Parser.char 'a',
        s <$ Parser.char 's'
      ]
  op <- ((<) <$ Parser.char '<') <|> ((>) <$ Parser.char '>')
  n <- Parser.decimal <* Parser.char ':'
  dest <- parseDestination
  pure $ \r part -> if field part `op` n then dest else r part

parseDestination :: Parser Destination
parseDestination =
  Parser.choice
    [ Accept <$ Parser.char 'A',
      Reject <$ Parser.char 'R',
      Next <$> parseName
    ]

parsePart :: Parser Part
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

solve1 :: HashMap Name Rule -> [Part] -> Int
solve1 workflows = sum . map add . filter (apply workflows)
  where
    add Part {x, m, a, s} = x + m + a + s

apply :: HashMap Name Rule -> Part -> Bool
apply workflows = go (workflows HashMap.! "in")
  where
    go rule part = case rule part of
      Next name -> go (workflows HashMap.! name) part
      Reject -> False
      Accept -> True
