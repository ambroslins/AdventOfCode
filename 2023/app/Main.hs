module Main (main) where

import AdventOfCode.Main (solve, solveAll)
import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [] -> solveAll
    args -> forM_ args $ \arg -> case readMaybe arg of
      Nothing -> putStrLn ("Can not parse : " <> arg)
      Just day -> solve day
