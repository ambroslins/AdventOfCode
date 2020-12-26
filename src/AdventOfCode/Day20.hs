module AdventOfCode.Day20 where

import AdventOfCode.Prelude
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

solution :: Solution
solution = Solution parseInput solve1 (const (0 :: Int))

data Tile = Tile
  { top :: Int,
    right :: Int,
    bottom :: Int,
    left :: Int
  }
  deriving (Eq, Ord, Show)

tileSize :: Int
tileSize = 10

binToDec :: [Bool] -> Int
binToDec = sum . zipWith (\x b -> if b then x else 0) (iterate (* 2) 1)

decToBin :: Int -> [Bool]
decToBin = take tileSize . map ((== 1) . snd) . iterate (quotRem2 . fst) . quotRem2
  where
    quotRem2 = (`quotRem` 2)

reverseBits :: Int -> Int
reverseBits = binToDec . reverse . decToBin

toTile :: [[Bool]] -> Tile
toTile xs =
  Tile
    { top = binToDec $ head xs,
      right = binToDec $ map last xs,
      bottom = binToDec $ last xs,
      left = binToDec $ map head xs
    }

rotate :: Tile -> Tile
rotate t =
  Tile
    { top = reverseBits $ left t,
      right = top t,
      bottom = reverseBits $ right t,
      left = bottom t
    }

flipV :: Tile -> Tile
flipV t =
  Tile
    { top = reverseBits $ top t,
      right = left t,
      bottom = reverseBits $ bottom t,
      left = right t
    }

flipH :: Tile -> Tile
flipH t =
  Tile
    { top = bottom t,
      right = reverseBits $ right t,
      bottom = top t,
      left = reverseBits $ left t
    }

parseTile :: Parser Tile
parseTile = toTile <$> some ((False <$ char '.') <|> (True <$ char '#')) `sepEndBy1` eol

parseInput :: Parser (IntMap Tile)
parseInput = fmap IntMap.fromList $
  (`sepEndBy` space1) $ do
    _ <- string "Tile "
    key <- decimal
    _ <- string ":" >> eol
    t <- parseTile
    pure (key, t)

variants :: Tile -> [Tile]
variants t =
  map
    ($ t)
    [ id,
      rotate,
      rotate . rotate,
      rotate . rotate . rotate,
      flipH,
      flipV,
      flipH . rotate,
      flipV . rotate
    ]

solve1 :: IntMap Tile -> Int
solve1 ts =
  product $
    map
      (fst . ($ image))
      [ head . head,
        head . last,
        last . head,
        last . last
      ]
  where
    tiles = variants <$> ts
    image =
      snd $
        head $
          IntMap.toList $
            IntMap.mapMaybeWithKey (\k -> asum . map (\t -> buildImage (imageSize - 1) (IntMap.delete k tiles) (k, t))) tiles

imageSize :: Int
imageSize = 12

buildSlice :: Int -> IntMap [Tile] -> Tile -> (Tile -> Int) -> (Tile -> Int) -> Maybe [(Int, Tile)]
buildSlice n tiles tile side otherSide =
  if n <= 0
    then Just []
    else case IntMap.toList $ IntMap.mapMaybe (find (\t -> side tile == otherSide t)) tiles of
      [] -> Nothing
      [(k, t)] -> ((k, t) :) <$> buildSlice (n - 1) (IntMap.delete k tiles) t side otherSide
      _ -> error "give up"

buildImage :: Int -> IntMap [Tile] -> (Int, Tile) -> Maybe [[(Int, Tile)]]
buildImage size tiles (key, tile) = case ((key, tile) :) <$> buildSlice size (IntMap.delete key tiles) tile right left of
  Nothing -> Nothing
  Just topRow -> go tiles' topRow
    where
      tiles' = IntMap.withoutKeys tiles $ IntSet.fromList $ map fst topRow
      go _ [] = Just []
      go ts ((k, t) : xs) = case ((k, t) :) <$> buildSlice size ts t bottom top of
        Nothing -> Nothing
        Just c -> (c :) <$> go (IntMap.withoutKeys ts $ IntSet.fromList $ map fst c) xs
