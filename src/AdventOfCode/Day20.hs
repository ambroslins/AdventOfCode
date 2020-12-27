module AdventOfCode.Day20 where

import AdventOfCode.Prelude
import Control.Monad.State
import Data.Bifunctor (bimap)
import qualified Data.IntMap as IntMap
import Data.List (nub, transpose)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Vector as Vector

-- this is probably the worst program i have ever written

solution :: Solution
solution = Solution parseInput solve1 solve2

data Tile = Tile
  { top :: Int,
    right :: Int,
    bottom :: Int,
    left :: Int,
    inside :: Image
  }
  deriving (Eq, Ord, Show)

type Image = Matrix Bool

tileSize :: Int
tileSize = 10

boolsToInt :: [Bool] -> Int
boolsToInt = sum . zipWith (\x b -> if b then x else 0) (iterate (* 2) 1)

decToBin :: Int -> [Bool]
decToBin = take tileSize . map ((== 1) . snd) . iterate (quotRem2 . fst) . quotRem2
  where
    quotRem2 = (`quotRem` 2)

toTile :: [[Bool]] -> Tile
toTile xs =
  Tile
    { top = boolsToInt $ head xs,
      right = boolsToInt $ map last xs,
      bottom = boolsToInt $ last xs,
      left = boolsToInt $ map head xs,
      inside = fromJust $ Matrix.fromLists $ map (init . tail) . (init . tail) $ xs
    }

rotate :: [[Bool]] -> [[Bool]]
rotate = flipV . transpose

flipV :: [[Bool]] -> [[Bool]]
flipV = map reverse

flipH :: [[Bool]] -> [[Bool]]
flipH = reverse

parseTile :: Parser [[Bool]]
parseTile = some ((False <$ char '.') <|> (True <$ char '#')) `sepEndBy1` eol

parseInput :: Parser (IntMap [Tile])
parseInput = fmap (fmap (map toTile . variants) . IntMap.fromList) $
  (`sepEndBy` space1) $ do
    _ <- string "Tile "
    key <- decimal
    _ <- string ":" >> eol
    t <- parseTile
    pure (key, t)

variants :: [[Bool]] -> [[[Bool]]]
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

solve1 :: IntMap [Tile] -> Int
solve1 tiles =
  product $
    map
      (fst . ($ image tiles))
      [ head . head,
        head . last,
        last . head,
        last . last
      ]

solve2 :: IntMap [Tile] -> Int
solve2 tiles =
  minimum $
    map
      ( (\i -> hashtags - monsters i) . (Vector.fromList . map Vector.fromList)
      )
      (variants img)
  where
    img' = map (map (inside . snd)) $ image tiles
    img = Matrix.toLists $ foldl1 Matrix.joinVertical $ map (foldl1 Matrix.joinHorizontal) img'
    n = length img
    hashtags = sum $ map (length . filter id) img
    monster =
      [ "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      ]
    offsets =
      concatMap
        (\(y, xs) -> mapMaybe (\(x, c) -> if c == '#' then Just (x, y) else Nothing) $ zip [0 ..] xs)
        $ zip [0 ..] monster
    monsters i =
      length $
        nub $
          concat
            [ bimap (+ x) (+ y) <$> offsets
              | x <- [0 .. n - 1],
                y <- [0 .. n - 1],
                all (\(dx, dy) -> Just True == ((i Vector.!? (y + dy)) >>= (Vector.!? (x + dx)))) offsets
            ]

image :: IntMap [Tile] -> [[(Int, Tile)]]
image tiles =
  snd $
    head $
      IntMap.toList $
        IntMap.mapMaybeWithKey (\k -> asum . map (\t -> buildImage imageSize (IntMap.delete k tiles) (k, t))) tiles

imageSize :: Int
imageSize = 12

buildSlice :: (Tile -> Int) -> (Tile -> Int) -> Int -> (Int, Tile) -> State (IntMap [Tile]) (Maybe [(Int, Tile)])
buildSlice from to size (key, tile) = do
  modify $ IntMap.delete key
  sequence . take size . (Just (key, tile) :) <$> go (to tile)
  where
    go :: Int -> State (IntMap [Tile]) [Maybe (Int, Tile)]
    go side = do
      tiles <- get
      case IntMap.toList $ IntMap.mapMaybe (find (\t -> side == from t)) tiles of
        [] -> pure [Nothing]
        [(k, t)] -> do
          modify (IntMap.delete k)
          (Just (k, t) :) <$> go (to t)
        _ -> undefined

buildRow :: Int -> (Int, Tile) -> State (IntMap [Tile]) (Maybe [(Int, Tile)])
buildRow = buildSlice left right

buildColumn :: Int -> (Int, Tile) -> State (IntMap [Tile]) (Maybe [(Int, Tile)])
buildColumn = buildSlice top bottom

buildImage :: Int -> IntMap [Tile] -> (Int, Tile) -> Maybe [[(Int, Tile)]]
buildImage size tiles start = flip evalState tiles $ do
  fstCol <- buildColumn size start
  join <$> mapM (fmap sequence . mapM (buildRow size)) fstCol

{-}
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

-}