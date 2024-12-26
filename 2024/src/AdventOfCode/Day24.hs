module AdventOfCode.Day24 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bits (xor)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List

type Name = ByteString

data Op = And | Or | Xor
  deriving (Eq, Ord, Show)

data Gate a = Gate {op :: !Op, lhs :: !a, rhs :: !a}
  deriving (Eq, Ord, Show)

solution :: Solution
solution =
  Solution
    { parser = do
        inputs <- parseInput `sepEndBy'` Parser.endOfLine
        Parser.endOfLine
        gates <- parseGate `sepEndBy'` Parser.endOfLine
        pure (inputs, gates),
      solver = uncurry solve
    }

parseName :: Parser Name
parseName = Parser.takeWhile1 Char.isAlphaNum

parseInput :: Parser (Name, Bool)
parseInput = do
  name <- parseName
  Parser.symbol ": "
  value <- False <$ Parser.char '0' <|> True <$ Parser.char '1'
  pure (name, value)

parseGate :: Parser (Name, Gate Name)
parseGate = do
  lhs <- parseName
  op <-
    choice
      [ And <$ Parser.symbol " AND ",
        Or <$ Parser.symbol " OR ",
        Xor <$ Parser.symbol " XOR "
      ]
  rhs <- parseName
  Parser.symbol " -> "
  output <- parseName
  pure (output, Gate {op, lhs, rhs})

solve :: [(Name, Bool)] -> [(Name, Gate Name)] -> (Int, Int)
solve inputs gates =
  ( foldl' (\acc n -> if eval n then acc * 2 + 1 else acc * 2) 0 outputs,
    0
  )
  where
    vars = HashMap.fromList inputs
    gs = HashMap.fromList gates
    outputs =
      List.sortBy (flip compare) $
        filter (BS.isPrefixOf "z") $
          map fst gates
    eval name = case HashMap.lookup name gs of
      Nothing -> vars HashMap.! name
      Just Gate {op, lhs, rhs} ->
        let l = eval lhs
            r = eval rhs
         in case op of
              And -> l && r
              Or -> l || r
              Xor -> l `xor` r
