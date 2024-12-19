module AdventOfCode.Day19 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAsciiLower)
import Data.Char qualified as Char
import Data.IntMap.Strict qualified as IntMap
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed.Mutable qualified as MVU

type Towel = ByteString

type Design = ByteString

data Trie = Trie !Bool !(IntMap.IntMap Trie)
  deriving (Show)

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
    trie = foldl' (flip insertTrie) emptyTrie towels
    go design =
      let options = possibleOptions trie design
       in Pair (if options > 0 then 1 else 0) (Sum options)

parseTowel :: Parser Towel
parseTowel = Parser.takeWhile1 isAsciiLower

parseDesing :: Parser Design
parseDesing = Parser.takeWhile1 isAsciiLower

possibleOptions :: Trie -> Design -> Int
possibleOptions trie design = runST $ do
  memo <- MVU.replicate (BS.length design + 1) (-1)
  MVU.write memo 0 1
  let go !d =
        MVU.read memo (BS.length d) >>= \case
          m
            | m >= 0 -> pure m
            | otherwise -> do
                s <- sum <$> mapM go (triePrefixes trie d)
                MVU.write memo (BS.length d) s
                pure s
  go design

emptyTrie :: Trie
emptyTrie = Trie False IntMap.empty

insertTrie :: ByteString -> Trie -> Trie
insertTrie bs (Trie final nodes) = case BS.uncons bs of
  Nothing -> Trie True nodes
  Just (c, cs) ->
    Trie final $
      IntMap.alter
        (Just . insertTrie cs . fromMaybe emptyTrie)
        (c2i c)
        nodes

triePrefixes :: Trie -> ByteString -> [ByteString]
triePrefixes (Trie final nodes) bs = case BS.uncons bs of
  Nothing -> [BS.empty | final]
  Just (c, cs) ->
    (if final then (bs :) else id) $
      maybe [] (`triePrefixes` cs) $
        IntMap.lookup (c2i c) nodes

c2i :: Char -> Int
c2i c = Char.ord c - Char.ord 'a'
