{-# OPTIONS_GHC -Wno-x-partial #-}

module AdventOfCode.Day09 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST.Strict (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Vector.Mutable qualified as MVector

data File = File
  { fileId :: !Int,
    fileStart :: !Int,
    fileSize :: !Int
  }
  deriving (Show)

fileEnd :: File -> Int
fileEnd f = fileStart f + fileSize f

solution :: Solution
solution =
  Solution
    { parser = decode <$> (Parser.line <* Parser.endOfLine),
      solver = uncurry solve1 &&& uncurry solve2
    }

decode :: ByteString -> ([File], IntMap Int)
decode =
  go 0 0 [] IntMap.empty
    . map (\c -> Char.ord c - Char.ord '0')
    . BS.unpack
  where
    go :: Int -> Int -> [File] -> IntMap Int -> [Int] -> ([File], IntMap Int)
    go !nextId !offset files frees = \case
      (size : free : rest) ->
        let f = File {fileId = nextId, fileStart = offset, fileSize = size}
         in go
              (nextId + 1)
              (offset + size + free)
              (f : files)
              (if free > 0 then IntMap.insert (offset + size) free frees else frees)
              rest
      [size] ->
        let f = File {fileId = nextId, fileStart = offset, fileSize = size}
         in (f : files, frees)
      [] -> (files, frees)

solve1 :: [File] -> IntMap Int -> Int
solve1 = flip (go 0)
  where
    go !acc frees = \case
      [] -> acc
      f : files -> case IntMap.minViewWithKey frees of
        Nothing -> sum $ map checksumFile (f : files)
        Just ((offset, free), frees')
          | offset >= fileStart f -> go (acc + checksumFile f) frees files
          | free >= fileSize f ->
              let newFile = f {fileStart = offset}
                  rest = free - fileSize f
               in go
                    (acc + checksumFile f {fileStart = offset})
                    ( if rest == 0
                        then frees'
                        else IntMap.insert (fileEnd newFile) rest frees'
                    )
                    files
          | otherwise ->
              let f1 = f {fileStart = offset, fileSize = free}
                  f2 = f {fileSize = fileSize f - free}
               in go (acc + checksumFile f1) frees' (f2 : files)

solve2 :: [File] -> IntMap Int -> Int
solve2 files frees = runST $ do
  let end = fileEnd $ head files
  buckets <- MVector.replicate 10 []
  forM_ (IntMap.toDescList frees) $
    \(offset, free) -> MVector.modify buckets (offset :) free

  let findBucket !size =
        MVector.ifoldl'
          ( \(i, offset) j os -> case os of
              o : _ | o < offset -> (j + size, o)
              _ -> (i, offset)
          )
          (0, end)
          (MVector.drop size buckets)

  let go !acc !f = do
        (bucketSize, bucketOffset) <- findBucket (fileSize f)
        if bucketSize == 0 || bucketOffset >= fileStart f
          then pure (acc + checksumFile f)
          else do
            let rest = bucketSize - fileSize f
            MVector.modify buckets tail bucketSize
            when (rest > 0) $
              MVector.modify
                buckets
                (List.insert (bucketOffset + fileSize f))
                rest
            pure (acc + checksumFile f {fileStart = bucketOffset})

  foldM go 0 files

checksumFile :: File -> Int
checksumFile f = sum $ map (* fileId f) [fileStart f .. fileEnd f - 1]
