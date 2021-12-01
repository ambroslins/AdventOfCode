module AdventOfCode.Day23 where

import AdventOfCode.Prelude
import Control.Monad (foldM_, replicateM_)
import Control.Monad.ST
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.STRef
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

solution :: Solution
solution = simpleSolution parseInput solve1 solve2

parseInput :: String -> [Int]
parseInput = map digitToInt . takeWhile isDigit

solve1 :: [Int] -> String
solve1 input = map intToDigit $ take (length input - 1) $ tail $ play 100 input

solve2 :: [Int] -> Int
solve2 input = product $ take 3 $ play 10000000 (input ++ [maximum input + 1 .. 1000000])

-- inspired by https://www.reddit.com/r/haskell/comments/kirxip/advent_of_code_2020_day_23_spoilers/ggshf65?utm_source=share&utm_medium=web2x&context=3
play :: Int -> [Int] -> [Int]
play n input = go 0
  where
    xs = map (subtract 1) input
    go x = let y = output Vector.! x in 1 + x : go y
    output = runST $ do
      let size = length xs
      vec <- MVector.new size
      foldM_ (\i v -> MVector.write vec v i >> pure v) (head xs) $ reverse xs
      currentRef <- newSTRef $ head xs

      let next = MVector.read vec
          setNext = MVector.write vec
          step = do
            current <- readSTRef currentRef
            c1 <- next current
            c2 <- next c1
            c3 <- next c2
            current' <- next c3
            -- skip the 3 picked up cups
            current `setNext` current'
            let decrement 0 = size - 1
                decrement x = x - 1
                destination = until (`notElem` [c1, c2, c3]) decrement (decrement current)
            prev <- next destination
            destination `setNext` c1
            c3 `setNext` prev
            writeSTRef currentRef current'
      replicateM_ n step
      Vector.freeze vec
