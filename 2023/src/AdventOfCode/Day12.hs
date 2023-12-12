module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict qualified as HashMap

type Cache = HashMap (Int, Int) Int

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
  flip State.evalState HashMap.empty . go bs
  where
    go :: ByteString -> [Int] -> State Cache Int
    go springs groups =
      State.gets (HashMap.lookup key) >>= \case
        Just n -> pure n
        Nothing -> do
          n <- recurse springs groups
          State.modify (HashMap.insert key n)
          pure n
      where
        key = (BS.length springs, length groups)
    recurse :: ByteString -> [Int] -> State Cache Int
    recurse springs groups =
      case groups of
        []
          | '#' `BS.elem` springs -> pure 0
          | otherwise -> pure 1
        g : gs -> case BS.uncons $ BS.dropWhile (== '.') springs of
          Just ('#', rest) -> closeGroup rest
          Just ('?', rest) -> do
            n1 <- closeGroup rest
            n2 <- go rest groups
            pure (n1 + n2)
          _ -> pure 0
          where
            closeGroup rest
              | BS.length rest < g - 1 || '.' `BS.elem` damaged = pure 0
              | otherwise = case BS.uncons operational of
                  Nothing | null gs -> pure 1
                  Just (x, xs) | x /= '#' -> go xs gs
                  _ -> pure 0
              where
                (damaged, operational) = BS.splitAt (g - 1) rest

solve1 :: [(ByteString, [Int])] -> Int
solve1 = sum . map (uncurry arrangements)

solve2 :: [(ByteString, [Int])] -> Int
solve2 = sum . parMap rseq (uncurry arrangements . unfold)

unfold :: (ByteString, [Int]) -> (ByteString, [Int])
unfold (springs, groups) =
  ( BS.intercalate "?" (replicate 5 springs),
    concat (replicate 5 groups)
  )
