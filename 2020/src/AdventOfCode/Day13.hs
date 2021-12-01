module AdventOfCode.Day13 where

import AdventOfCode.Prelude
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

data BusTabel = BusTabel Integer [Maybe Integer]
  deriving (Eq, Show)

parseBusTabel :: Parser BusTabel
parseBusTabel =
  BusTabel
    <$> (decimal <* eol)
    <*> (Just <$> decimal <|> Nothing <$ char 'x') `sepBy` char ','

solution :: Solution
solution = Solution parseBusTabel solve1 solve2

solve1 :: BusTabel -> Integer
solve1 (BusTabel x xs) =
  uncurry (*) $
    minimumBy (compare `on` snd) $
      mapMaybe (fmap $ \y -> (y, y - (x `mod` y))) xs

-- not a real solution, takes way too long.
solve2 :: BusTabel -> Integer
solve2 (BusTabel _ ms) = crt equations
  where
    equations = mapMaybe (\(a, b) -> (- a,) <$> b) $ zip [0 ..] ms

crt :: [(Integer, Integer)] -> Integer
crt xs =
  (`mod` m) $
    sum $
      map (\(a, n) -> let b = m `div` n; (_, b', _) = egcd b n in a * b * b') xs
  where
    m = product $ map snd xs

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, x, y) = egcd (b `mod` a) a in (g, y - (b `div` a) * x, x)
