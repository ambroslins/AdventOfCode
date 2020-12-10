module Main where

import AdventOfCode
import AdventOfCode.Prelude
import Control.Monad (forM_)
import qualified Data.Map as Map
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [] -> runAllSolutions
    xs -> forM_ xs $ \arg -> case Day <$> readMaybe arg of
      Nothing -> putStrLn $ arg ++ " is not a number"
      Just d ->
        maybe (putStrLn $ show d ++ " not implemented") (runSolution d) $
          Map.lookup d solutions
