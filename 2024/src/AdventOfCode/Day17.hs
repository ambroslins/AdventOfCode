module AdventOfCode.Day17 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Vector.Unboxed qualified as VU

data Computer a = Computer
  { regA :: !a,
    regB :: !a,
    regC :: !a
  }
  deriving (Show)

type Program = VU.Vector Int

solution :: Solution
solution =
  Solution
    { parser = do
        c <- parseComputer
        Parser.endOfLine
        p <- VU.fromList <$> parseProgram
        Parser.endOfLine
        pure (c, p),
      solver = uncurry solve1 &&& quine . snd
    }

parseComputer :: Parser (Computer Int)
parseComputer = do
  regA <- parseReg "A"
  regB <- parseReg "B"
  regC <- parseReg "C"
  pure Computer {regA, regB, regC}
  where
    parseReg l = do
      Parser.symbol "Register "
      Parser.symbol l
      Parser.symbol ": "
      Parser.decimal <* Parser.endOfLine

parseProgram :: Parser [Int]
parseProgram = do
  Parser.symbol "Program: "
  Parser.decimal `sepBy'` Parser.char ','

-- Be careful of leading zeros
solve1 :: Computer Int -> Program -> Int
solve1 computer program =
  foldl' (\acc x -> acc * 10 + x) 0 $
    run computer program

run :: Computer Int -> Program -> [Int]
run computer program = go computer 0
  where
    go !c !ip
      | ip >= VU.length program = []
      | otherwise = case opcode of
          0 -> go (c {regA = regA c `shiftR` combo}) (ip + 2)
          1 -> go (c {regB = regB c `xor` operand}) (ip + 2)
          2 -> go (c {regB = combo .&. 7}) (ip + 2)
          3 -> if regA c == 0 then go c (ip + 2) else go c operand
          4 -> go (c {regB = regB c `xor` regC c}) (ip + 2)
          5 -> (combo .&. 7) : go c (ip + 2)
          6 -> go (c {regB = regA c `shiftR` combo}) (ip + 2)
          7 -> go (c {regC = regA c `shiftR` combo}) (ip + 2)
          _ -> error $ "unexpected opcode: " <> show opcode
      where
        opcode = program VU.! ip
        operand = program VU.! (ip + 1)
        combo = case operand of
          op | op <= 3 -> op
          4 -> regA c
          5 -> regB c
          6 -> regC c
          op -> error $ "unexpected operand: " <> show op

quine :: Program -> Int
quine program = case go (VU.toList program) of
  [] -> error "quine: no solution found"
  a : _ -> a
  where
    go outputs = case outputs of
      [] -> [0]
      o : os ->
        [ a'
        | a <- go os,
          r <- [0 .. 7],
          let a' = (a `shiftL` 3) .|. r,
          let c = Computer {regA = a', regB = 0, regC = 0},
          Just o == listToMaybe (run c program)
        ]
