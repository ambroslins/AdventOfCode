module Main where

import AdventOfCode.Main (readInputFile, solutions)
import AdventOfCode.Parser (runParser)
import AdventOfCode.Prelude (Solution (..))
import Data.IntMap.Strict qualified as IntMap
import Test.Tasty.Bench
import Text.Printf (printf)

main :: IO ()
main = defaultMain $ map (uncurry benchDay) $ IntMap.toList solutions

benchDay :: Int -> Solution -> Benchmark
benchDay day (Solution {parser, solver}) = env (readInputFile day) run
  where
    run input =
      bgroup
        (printf "Day %02d" day)
        [ bench "parse" $ whnf (runParser parser) input,
          bench "part 1" $ whnf (fst . solver) parsedInput,
          bench "part 2" $ whnf (snd . solver) parsedInput,
          bench "total" $ whnf (forceTuple . total) input
        ]
      where
        parsedInput = runParser parser input

    total = solver . runParser parser

forceTuple :: (a, b) -> (a, b)
forceTuple (!a, !b) = (a, b)
