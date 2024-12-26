{-# OPTIONS_GHC -Wno-x-partial #-}

module AdventOfCode.Day09 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST.Strict (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
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

data Space = Space
  { spaceStart :: !Int,
    spaceSize :: !Int
  }

solution :: Solution
solution =
  Solution
    { parser = decode <$> (Parser.line <* Parser.endOfLine),
      solver = uncurry solve1 &&& uncurry solve2
    }

decode :: ByteString -> ([File], [Space])
decode =
  go 0 0 [] []
    . map (\c -> Char.ord c - Char.ord '0')
    . BS.unpack
  where
    go !nextId !offset files blanks = \case
      (size : free : rest) ->
        let f = File {fileId = nextId, fileStart = offset, fileSize = size}
            b = Space {spaceStart = offset + size, spaceSize = free}
         in go
              (nextId + 1)
              (offset + size + free)
              (f : files)
              (if free > 0 then b : blanks else blanks)
              rest
      [size] ->
        let f = File {fileId = nextId, fileStart = offset, fileSize = size}
         in (f : files, blanks)
      [] -> (files, blanks)

solve1 :: [File] -> [Space] -> Int
solve1 files' spaces' = go 0 (reverse spaces') files'
  where
    go !acc spaces = \case
      [] -> acc
      f : files -> case spaces of
        [] -> sum $ map checksumFile (f : files)
        b : bs
          | spaceStart b >= fileStart f -> go (acc + checksumFile f) spaces files
          | spaceSize b >= fileSize f ->
              let newFile = f {fileStart = spaceStart b}
                  rest = spaceSize b - fileSize f
                  newspace = Space {spaceStart = fileEnd newFile, spaceSize = rest}
               in go
                    (acc + checksumFile f {fileStart = spaceStart b})
                    ( if rest == 0
                        then bs
                        else newspace : bs
                    )
                    files
          | otherwise ->
              let f1 = f {fileStart = spaceStart b, fileSize = spaceSize b}
                  f2 = f {fileSize = fileSize f - spaceSize b}
               in go (acc + checksumFile f1) bs (f2 : files)

solve2 :: [File] -> [Space] -> Int
solve2 files blanks = runST $ do
  let end = fileEnd $ head files
  buckets <- MVector.replicate 10 []
  forM_ blanks $
    \Space {spaceStart, spaceSize} ->
      MVector.modify buckets (spaceStart :) spaceSize

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
