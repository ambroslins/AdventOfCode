module AdventOfCode where

import qualified AdventOfCode.Day01 as Day01
import qualified AdventOfCode.Day02 as Day02
import qualified AdventOfCode.Day03 as Day03
import qualified AdventOfCode.Day04 as Day04
import qualified AdventOfCode.Day05 as Day05
import qualified AdventOfCode.Day06 as Day06
import qualified AdventOfCode.Day07 as Day07
import qualified AdventOfCode.Day08 as Day08
import qualified AdventOfCode.Day09 as Day09
import qualified AdventOfCode.Day10 as Day10
import qualified AdventOfCode.Day11 as Day11
import qualified AdventOfCode.Day12 as Day12
import qualified AdventOfCode.Day13 as Day13
import qualified AdventOfCode.Day14 as Day14
import qualified AdventOfCode.Day15 as Day15
import qualified AdventOfCode.Day16 as Day16
import qualified AdventOfCode.Day17 as Day17
import AdventOfCode.Prelude
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Network.HTTP.Client (Cookie (..), createCookieJar)
import Network.HTTP.Req
import System.Directory
import System.FilePath
import Text.Megaparsec (eof, errorBundlePretty, parse)

solutions :: Map Day Solution
solutions =
  Map.fromList
    [ (Day 1, Day01.solution),
      (Day 2, Day02.solution),
      (Day 3, Day03.solution),
      (Day 4, Day04.solution),
      (Day 5, Day05.solution),
      (Day 6, Day06.solution),
      (Day 7, Day07.solution),
      (Day 8, Day08.solution),
      (Day 9, Day09.solution),
      (Day 10, Day10.solution),
      (Day 11, Day11.solution),
      (Day 12, Day12.solution),
      (Day 13, Day13.solution),
      (Day 14, Day14.solution),
      (Day 15, Day15.solution),
      (Day 16, Day16.solution),
      (Day 17, Day17.solution)
    ]

runSolution :: Day -> Solution -> IO ()
runSolution day (Solution p s1 s2) = do
  input <- getInput day
  case parse (p <* space <* eof) (show day) input of
    Left e -> putStrLn $ errorBundlePretty e
    Right x -> do
      print day
      print $ s1 x
      print $ s2 x

runAllSolutions :: IO ()
runAllSolutions = mapM_ (uncurry runSolution) $ Map.toList solutions

getInput :: Day -> IO Text
getInput (Day d) = fromFile >>= maybe fromWebsite pure
  where
    file = "input" </> show d <.> "txt"
    fromFile = (Just <$> Text.readFile file) `catch` (\e -> let _ = (e :: IOException) in pure Nothing)
    fromWebsite = do
      cookieValue <- BS.takeWhile isAlphaNum <$> BS.readFile "cookie"
      now <- getCurrentTime
      let cookie =
            Cookie
              "session"
              cookieValue
              (addUTCTime (nominalDay * 30) now)
              "adventofcode.com"
              "/"
              now
              now
              True
              True
              False
              False
          options = cookieJar $ createCookieJar [cookie]
      r <-
        runReq defaultHttpConfig $
          req
            GET
            (https "adventofcode.com" /: "2020" /: "day" /: Text.pack (show d) /: "input")
            NoReqBody
            bsResponse
            options
      let input = responseBody r
      createDirectoryIfMissing True $ takeDirectory file
      BS.writeFile file input
      pure $ decodeUtf8 input
