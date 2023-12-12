module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict qualified as HashMap

type Cache = HashMap (Int, Int, Int) Int

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseLine :: Parser (ByteString, [Int])
parseLine = do
  springs <- Parser.takeWhile1 (/= ' ') <* Parser.space
  groups <- Parser.decimal `sepEndBy'` Parser.char ','
  pure (springs, groups)

arrangements :: ByteString -> [Int] -> Int
arrangements bs =
  flip State.evalState HashMap.empty . go 0 bs
  where
    go :: Int -> ByteString -> [Int] -> State Cache Int
    go !c springs groups =
      State.gets (HashMap.lookup key) >>= \case
        Just n -> pure n
        Nothing -> do
          n <- recurse c springs groups
          State.modify (HashMap.insert key n)
          pure n
      where
        key = (c, BS.length springs, length groups)
    recurse :: Int -> ByteString -> [Int] -> State Cache Int
    recurse !c springs groups =
      case groups of
        []
          | '#' `BS.elem` springs -> pure 0
          | otherwise -> pure 1
        g : gs -> case BS.uncons springs of
          Nothing | c == g && null gs -> pure 1
          Just ('.', rest)
            | c == 0 -> continueOperational rest
            | c == g -> closeGroup rest
          Just ('#', rest)
            | c < g -> continueGroup rest
          Just ('?', rest)
            | c == 0 -> (+) <$> continueGroup rest <*> continueOperational rest
            | c == g -> closeGroup rest
            | otherwise -> continueGroup rest
          _ -> pure 0
          where
            closeGroup rest = go 0 (BS.dropWhile (== '.') rest) gs
            continueGroup rest = go (c + 1) rest groups
            continueOperational rest = go 0 (BS.dropWhile (== '.') rest) groups

solve1 :: [(ByteString, [Int])] -> Int
solve1 = sum . map (uncurry arrangements)

solve2 :: [(ByteString, [Int])] -> Int
solve2 = sum . parMap rseq (uncurry arrangements . unfold)

unfold :: (ByteString, [Int]) -> (ByteString, [Int])
unfold (springs, groups) =
  ( BS.intercalate "?" (replicate 5 springs),
    concat (replicate 5 groups)
  )
