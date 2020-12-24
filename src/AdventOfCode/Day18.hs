module AdventOfCode.Day18 where

import AdventOfCode.Prelude
import Control.Monad.Combinators.Expr
import Text.Megaparsec (parse, takeRest)

solution :: Solution
solution = Solution takeRest (solve parser1) (solve parser2)
  where
    solve :: Parser Integer -> Text -> Integer
    solve p = either (const 0) sum . parse (p `sepEndBy` eol) ""

term :: Parser Integer -> Parser Integer
term p = lexeme $ between (char '(') (char ')') p <|> decimal

parser1 :: Parser Integer
parser1 =
  makeExprParser
    (term parser1)
    [ [InfixL ((+) <$ lexeme (char '+')), InfixL ((*) <$ lexeme (char '*'))]
    ]

parser2 :: Parser Integer
parser2 =
  makeExprParser
    (term parser2)
    [ [InfixL ((+) <$ lexeme (char '+'))],
      [InfixL ((*) <$ lexeme (char '*'))]
    ]
