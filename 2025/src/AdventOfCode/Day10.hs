module AdventOfCode.Day10 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bits (setBit, shiftL, xor)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Semigroup (Min (..))
import Data.Word (Word32)

solution :: Solution
solution =
  Solution
    { parser = parseMachine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

data Machine = Machine
  { lights :: !Word32,
    buttons :: [Word32],
    joltage :: [Word32]
  }
  deriving (Show)

parseMachine :: Parser Machine
parseMachine = do
  lights <-
    between (Parser.char '[') (Parser.char ']') $
      BS.foldr' (\c acc -> acc `shiftL` 1 + if c == c2w '#' then 1 else 0) 0
        <$> Parser.takeWhile1 (/= ']')
  Parser.whitespace
  buttons <- parseButton `sepEndBy'` Parser.whitespace
  joltage <-
    between (Parser.char '{') (Parser.char '}') $
      toList <$> Parser.decimal `sepBy1'` Parser.char ','
  pure Machine {lights, buttons, joltage}
  where
    parseButton =
      between (Parser.char '(') (Parser.char ')') $
        foldl' setBit 0
          <$> Parser.decimal `sepBy1'` Parser.char ','

solve :: [Machine] -> (Int, Int)
solve machines = (sum $ map presses machines, 0)

presses :: Machine -> Int
presses Machine {lights, buttons} = getMin $ go 0 lights buttons
  where
    go !n 0 _ = Min n
    go !_ _ [] = mempty
    go !n !acc (b : bs) = go n acc bs <> go (n + 1) (acc `xor` b) bs
