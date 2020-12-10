module AdventOfCode.Day02 where

import AdventOfCode.Prelude

data Password = Password (Int, Int) Char String
  deriving (Show)

parsePassword :: Parser Password
parsePassword = do
  x <- decimal
  char '-'
  y <- decimal
  space
  c <- letterChar
  char ':'
  space
  s <- some letterChar
  pure $ Password (x, y) c s

solution :: Solution
solution = Solution (parsePassword `sepEndBy` eol) solve1 solve2

solve1 :: [Password] -> Int
solve1 = length . filter isVaildPassword1

solve2 :: [Password] -> Int
solve2 = length . filter isVaildPassword2

isVaildPassword1 :: Password -> Bool
isVaildPassword1 (Password (x, y) c s) = x <= n && n <= y
  where
    n = length $ filter (== c) s

isVaildPassword2 :: Password -> Bool
isVaildPassword2 (Password (x, y) c s) = [c] == filter (== c) [s !! (x -1), s !! (y -1)]
