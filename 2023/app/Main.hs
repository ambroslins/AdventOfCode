{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import AdventOfCode.Main (solve, today)
import Control.Monad (forM)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  days <-
    getArgs >>= \case
      [] -> sequence [today]
      args -> fmap catMaybes $ forM args $ \arg -> case readMaybe arg of
        Nothing -> putStrLn ("Can not parse day: " <> arg) $> Nothing
        Just day -> pure $ Just day
  traverse_ solve days
