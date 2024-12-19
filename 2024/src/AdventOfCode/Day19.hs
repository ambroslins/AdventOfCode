module AdventOfCode.Day19 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed.Mutable qualified as MVU

type Towel = ByteString

type Design = ByteString

solution :: Solution
solution =
  Solution
    { parser = do
        towels <- parseTowel `sepBy'` Parser.symbol ", "
        Parser.endOfLine
        Parser.endOfLine
        designs <- parseDesing `sepEndBy'` Parser.endOfLine
        pure (towels, designs),
      solver = uncurry solve
    }

solve :: [Towel] -> [Design] -> (Int, Int)
solve towels designs = (p1, p2)
  where
    Pair (Sum p1) (Sum p2) = foldMap' go designs
    go design =
      let options = possibleOptions towels design
       in Pair (if options > 0 then 1 else 0) (Sum options)

parseTowel :: Parser Towel
parseTowel = Parser.takeWhile1 isAsciiLower

parseDesing :: Parser Design
parseDesing = Parser.takeWhile1 isAsciiLower

possibleOptions :: [Towel] -> Design -> Int
possibleOptions towels design = runST $ do
  memo <- MVU.replicate (BS.length design + 1) (-1)
  let go !d
        | BS.null d = pure 1
        | otherwise =
            MVU.read memo (BS.length d) >>= \case
              m
                | m >= 0 -> pure m
                | otherwise -> do
                    s <-
                      sum
                        <$> mapM go (mapMaybe (`BS.stripPrefix` d) towels)
                    MVU.write memo (BS.length d) s
                    pure s
  go design
