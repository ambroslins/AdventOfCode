module AdventOfCode.Day08 where

import AdventOfCode.Prelude
import qualified Data.Set as Set
import qualified Data.Vector as Vector

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int

parseInstruction :: Parser Instruction
parseInstruction =
  choice
    [ Nop <$> (symbol "nop" *> signed decimal),
      Acc <$> (symbol "acc" *> signed decimal),
      Jmp <$> (symbol "jmp" *> signed decimal)
    ]

data State = State
  { acc :: Int,
    pc :: Int
  }

startState :: State
startState = State 0 0

exec :: State -> Instruction -> State
exec s ins = case ins of
  Nop _ -> s'
  Acc i -> s' {acc = i + acc s'}
  Jmp i -> s {pc = i + pc s}
  where
    s' = s {pc = 1 + pc s}

step :: Vector Instruction -> State -> Maybe State
step is s = exec s <$> is Vector.!? pc s

solution :: Solution
solution = Solution (Vector.fromList <$> parseInstruction `sepEndBy` eol) solve1 solve2

solve1 :: Vector Instruction -> Int
solve1 is = go Set.empty startState
  where
    go executed s
      | pc s `Set.member` executed = acc s
      | otherwise = maybe (acc s) (go (Set.insert (pc s) executed)) (step is s)

solve2 :: Vector Instruction -> Int
solve2 is = Vector.head $ Vector.mapMaybe run iss
  where
    iss =
      Vector.imapMaybe
        ( \ix ins -> case ins of
            Acc _ -> Nothing
            Nop x -> Just $ is Vector.// [(ix, Jmp x)]
            Jmp x -> Just $ is Vector.// [(ix, Nop x)]
        )
        is

-- return ths acc if the program terminates, else nothing
run :: Vector Instruction -> Maybe Int
run is = go Set.empty startState
  where
    go executed s
      | pc s `Set.member` executed = Nothing
      | otherwise = case step is s of
        Nothing -> Just $ acc s
        Just s' -> go (Set.insert (pc s) executed) s'
