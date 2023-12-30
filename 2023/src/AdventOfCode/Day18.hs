module AdventOfCode.Day18 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, ord)

data Step = Step
  { dir :: !Direction,
    number :: !Int
  }
  deriving (Show)

solution :: Solution
solution =
  Solution
    { parser = parseStep `sepEndBy'` Parser.endOfLine,
      solver = solve . map fst &&& solve . map snd
    }

parseStep :: Parser (Step, Step)
parseStep = do
  step1 <- parseStep1
  step2 <- parseStep2
  pure (step1, step2)

parseStep1 :: Parser Step
parseStep1 = do
  dir <-
    Parser.lexeme $
      Parser.choice
        [ North <$ Parser.char 'U',
          South <$ Parser.char 'D',
          West <$ Parser.char 'L',
          East <$ Parser.char 'R'
        ]
  number <- Parser.lexeme Parser.decimal
  pure Step {dir, number}

parseStep2 :: Parser Step
parseStep2 = between "(#" (Parser.char ')') $ do
  number <- hex <$> Parser.take 5
  dir <-
    Parser.choice
      [ East <$ Parser.char '0',
        South <$ Parser.char '1',
        West <$ Parser.char '2',
        North <$ Parser.char '3'
      ]
  pure Step {number, dir}
  where
    hex = BS.foldl' f 0
    f a c
      | isDigit c = a * 16 + (ord c - ord '0')
      | otherwise = a * 16 + (ord c - ord 'a' + 10)

dig :: [Step] -> [Position]
dig = scanl step Position.origin
  where
    step pos Step {dir, number} = Position.moveN dir number pos

shoelace :: [Position] -> Int
shoelace ps = sum (zipWith triangle ps (tail ps)) `div` 2
  where
    triangle (Position x0 y0) (Position x1 y1) = x0 * y1 - x1 * y0

solve :: [Step] -> Int
solve steps = a + b `div` 2 + 1
  where
    positions = dig steps
    a = abs (shoelace positions)
    b = sum $ map number steps
