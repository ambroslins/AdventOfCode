module Main where

import AdventOfCode.Main (readInputFile, solutions)
import AdventOfCode.Parser (runParser)
import AdventOfCode.Prelude (Solution (..))
import Data.IntMap.Strict qualified as IntMap
import Test.Tasty.Bench

main :: IO ()
main = defaultMain $ map (uncurry benchDay) $ IntMap.toList solutions

benchDay :: Int -> Solution -> Benchmark
benchDay day (Solution {parser, part1, part2}) = env (readInputFile day) run
  where
    run input =
      bgroup
        ("Day " <> show day)
        [ bench "parse" $ whnf (runParser parser) input,
          bench "part 1" $ nf part1 parsedInput,
          bench "part 2" $ nf part2 parsedInput,
          bench "total" $ nf total input
        ]
      where
        parsedInput = runParser parser input

    total input = let x = runParser parser input in (part1 x, part2 x)
