module AdventOfCode.Day13 where

import AdventOfCode.Prelude
import Data.List (maximumBy, minimumBy)
import Data.Maybe (mapMaybe)

data BusTabel = BusTabel Integer [Maybe Integer]
  deriving (Eq, Show)

parseBusTabel :: Parser BusTabel
parseBusTabel =
  BusTabel
    <$> (decimal <* eol)
    <*> (Just <$> decimal <|> Nothing <$ char 'x') `sepBy` char ','

solution :: Solution
solution = Solution parseBusTabel solve1 (const ("failed" :: String))

solve1 :: BusTabel -> Integer
solve1 (BusTabel x xs) =
  uncurry (*) $
    minimumBy (compare `on` snd) $
      mapMaybe (fmap $ \y -> (y, y - (x `mod` y))) xs

-- not a real solution, takes way too long.
solve2 :: BusTabel -> Integer
solve2 (BusTabel _ ms) = head $ foldr (\(y, o) ys -> filter (f y o) ys) (iterate (+ bus) (bus - offset)) xs
  where
    (bus, offset) = maximumBy (compare `on` fst) xs
    xs = mapMaybe (\(a, b) -> (,b) <$> a) $ zip ms [0 ..]
    f x y z = (x - (z `mod` x)) `mod` x == y
