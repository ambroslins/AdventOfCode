module AdventOfCode.Day14 where

import AdventOfCode.Prelude
import Data.Bool (bool)
import qualified Data.IntMap as IntMap
import qualified Data.Vector as Vector

data BitMaskBit = Off | On | X
  deriving (Eq, Show)

type Mask = Vector BitMaskBit

type BitVector = Vector Bool

data Instruction = Mask Mask | Assign Int Int
  deriving (Eq, Show)

data Ferry = Ferry Mask (IntMap Int)
  deriving (Eq, Show)

parseMask :: Parser Mask
parseMask = string "mask = " >> Vector.fromList . reverse <$> count 36 parseBit
  where
    parseBit =
      choice
        [ Off <$ char '0',
          X <$ char 'X',
          On <$ char '1'
        ]

parseAssign :: Parser Instruction
parseAssign = do
  _ <- string "mem"
  adr <- between (char '[') (char ']') decimal
  _ <- string " = "
  Assign adr <$> decimal

parseInstruction :: Parser Instruction
parseInstruction = Mask <$> parseMask <|> parseAssign

toBitVector :: Int -> BitVector
toBitVector = Vector.fromListN 36 . map (f . snd) . iterate (quotRem2 . fst) . quotRem2
  where
    f 0 = False
    f 1 = True
    f _ = error "not zero or one"
    quotRem2 = (`quotRem` 2)

fromBitVector :: BitVector -> Int
fromBitVector = sum . zipWith (bool 0) (iterate (* 2) 1) . Vector.toList

applyBitMaskBit :: BitMaskBit -> Bool -> Bool
applyBitMaskBit = \case
  Off -> const False
  On -> const True
  X -> id

applyMask :: Mask -> BitVector -> BitVector
applyMask = Vector.zipWith applyBitMaskBit

solution :: Solution
solution = Solution (parseInstruction `sepEndBy` eol) solve1 solve2

exec :: Ferry -> Instruction -> Ferry
exec (Ferry mask memory) = \case
  Mask m -> Ferry m memory
  Assign addr value ->
    Ferry mask $
      IntMap.insert addr (fromBitVector $ applyMask mask $ toBitVector value) memory

startFerry :: Ferry
startFerry = Ferry undefined IntMap.empty

solve1 :: [Instruction] -> Int
solve1 xs = let (Ferry _ memory) = foldl exec startFerry xs in sum memory

solve2 :: [Instruction] -> Int
solve2 xs = let (Ferry _ memory) = foldl exec2 startFerry xs in sum memory

exec2 :: Ferry -> Instruction -> Ferry
exec2 (Ferry mask memory) = \case
  Mask m -> Ferry m memory
  Assign addr value -> Ferry mask $ foldr (`IntMap.insert` value) memory addrs
    where
      addrs = map (fromBitVector . Vector.fromList) $ floatAddresses $ Vector.toList $ applyMask2 mask $ toBitVector addr

applyMask2 :: Mask -> BitVector -> Mask
applyMask2 = Vector.zipWith $ \case
  Off -> bool Off On
  On -> const On
  X -> const X

floatAddresses :: [BitMaskBit] -> [[Bool]]
floatAddresses [] = [[]]
floatAddresses (x : xs) = case x of
  X -> map (False :) xs' ++ map (True :) xs'
  On -> map (True :) xs'
  Off -> map (False :) xs'
  where
    xs' = floatAddresses xs
