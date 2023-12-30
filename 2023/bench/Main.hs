module Main where

import AdventOfCode.Main (readInputFile, solutions)
import AdventOfCode.Parser (runParser)
import AdventOfCode.Prelude (Solution (..))
import Data.IntMap.Strict qualified as IntMap
import Test.Tasty.Bench

main :: IO ()
main = defaultMain $ map (uncurry benchDay) $ IntMap.toList solutions

benchDay :: Int -> Solution -> Benchmark
benchDay day (Solution {parser, solver}) = env (readInputFile day) run
  where
    run input =
      bgroup
        ("Day " <> show day)
        [ bench "parse" $ whnf (runParser parser) input,
          bench "part 1" $ nf (fst . solver) parsedInput,
          bench "part 2" $ nf (snd . solver) parsedInput,
          bench "total" $ nf total input
        ]
      where
        parsedInput = runParser parser input

    total = solver . runParser parser
