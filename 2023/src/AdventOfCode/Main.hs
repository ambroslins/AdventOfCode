module AdventOfCode.Main
  ( solve,
    solutions,
    today,
    solveToday,
    readInputFile,
  )
where

import AdventOfCode.Day01 qualified as Day01
import AdventOfCode.Day02 qualified as Day02
import AdventOfCode.Day03 qualified as Day03
import AdventOfCode.Day04 qualified as Day04
import AdventOfCode.Day05 qualified as Day05
import AdventOfCode.Day06 qualified as Day06
import AdventOfCode.Day07 qualified as Day07
import AdventOfCode.Day08 qualified as Day08
import AdventOfCode.Day09 qualified as Day09
import AdventOfCode.Day10 qualified as Day10
import AdventOfCode.Day11 qualified as Day11
import AdventOfCode.Day12 qualified as Day12
import AdventOfCode.Day13 qualified as Day13
import AdventOfCode.Day14 qualified as Day14
import AdventOfCode.Day15 qualified as Day15
import AdventOfCode.Day16 qualified as Day16
import AdventOfCode.Day17 qualified as Day17
import AdventOfCode.Day18 qualified as Day18
import AdventOfCode.Day19 qualified as Day19
import AdventOfCode.Day20 qualified as Day20
import AdventOfCode.Day21 qualified as Day21
import AdventOfCode.Day22 qualified as Day22
import AdventOfCode.Day23 qualified as Day23
import AdventOfCode.Day24 qualified as Day24
import AdventOfCode.Parser (runParser)
import AdventOfCode.Prelude
import Control.DeepSeq (force)
import Control.Exception (catch)
import Control.Exception.Base (throwIO)
import Data.ByteString qualified as BS
import Data.IntMap qualified as IntMap
import Data.Time
  ( NominalDiffTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDay,
    toGregorian,
    utctDay,
  )
import Network.HTTP.Client
  ( Cookie (..),
    Request (cookieJar),
    Response (responseBody, responseStatus),
    createCookieJar,
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (..))
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

solutions :: IntMap Solution
solutions =
  IntMap.fromList
    [ (1, Day01.solution),
      (2, Day02.solution),
      (3, Day03.solution),
      (4, Day04.solution),
      (5, Day05.solution),
      (6, Day06.solution),
      (7, Day07.solution),
      (8, Day08.solution),
      (9, Day09.solution),
      (10, Day10.solution),
      (11, Day11.solution),
      (12, Day12.solution),
      (13, Day13.solution),
      (14, Day14.solution),
      (15, Day15.solution),
      (16, Day16.solution),
      (17, Day17.solution),
      (18, Day18.solution),
      (19, Day19.solution),
      (20, Day20.solution),
      (21, Day21.solution),
      (22, Day22.solution),
      (23, Day23.solution),
      (24, Day24.solution)
    ]

readInputFile :: Int -> IO ByteString
readInputFile day = do
  BS.readFile path `catch` \e ->
    if isDoesNotExistError e
      then do
        input <- downloadInput day
        BS.writeFile path input
        pure input
      else throwIO e
  where
    path = "inputs/day" <> show day <> ".txt"

getCookie :: IO Cookie
getCookie = do
  cookieValue <- BS.readFile "cookie"
  now <- getCurrentTime
  pure $
    Cookie
      { cookie_name = "session",
        cookie_value = cookieValue,
        cookie_expiry_time = addUTCTime (nominalDay * 30) now,
        cookie_domain = "adventofcode.com",
        cookie_path = "/",
        cookie_creation_time = now,
        cookie_last_access_time = now,
        cookie_persistent = True,
        cookie_host_only = True,
        cookie_secure_only = False,
        cookie_http_only = False
      }

downloadInput :: Int -> IO ByteString
downloadInput day = do
  cookie <- getCookie
  let url = "https://adventofcode.com/2023/day/" <> show day <> "/input"
  putStrLn $ "Downloading input for day " <> show day <> "..."
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  let requestWithCookie = request {cookieJar = Just $ createCookieJar [cookie]}
  response <- httpLbs requestWithCookie manager
  let status = statusCode $ responseStatus response
  if status == 200
    then do
      putStrLn "Download done!"
      pure $ BS.toStrict $ responseBody response
    else
      throwIO $
        userError $
          "Failed to download input: "
            <> show status
            <> " "
            <> show (responseBody response)

-- | Get the current day of the month for the UTC-5 timezone.
today :: IO Int
today = do
  estTime <- addUTCTime (-5 * 3600) <$> getCurrentTime
  let (_, _, day) = toGregorian $ utctDay estTime
  pure day

-- | Run some function f and benchmark it with the give argument.
--
-- Returns the result of @f x@ and the time it took to run it.
-- Will evalute the result of @f x@ to WHNF before returning.
bench :: (a -> b) -> a -> IO (b, NominalDiffTime)
bench f x = do
  start <- x `seq` getCurrentTime
  let result = f x
  end <- result `seq` getCurrentTime
  pure (result, diffUTCTime end start)

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime diff
  | s < 1e-5 = printf "%.3f μs" us
  | s < 1e-4 = printf "%.2f μs" us
  | s < 1e-3 = printf "%.1f μs" us
  | s < 1e-2 = printf "%.3f ms" ms
  | s < 1e-1 = printf "%.2f ms" ms
  | s < 1e+0 = printf "%.1f ms" ms
  | s < 1e+1 = printf "%.3f s" s
  | s < 1e+2 = printf "%.2f s" s
  | otherwise = printf "%.1f s" s
  where
    s = realToFrac diff :: Double
    ms = s * 1e3
    us = s * 1e6

solve :: Int -> IO ()
solve day = do
  input <- readInputFile day
  case IntMap.lookup day solutions of
    Nothing -> putStrLn $ "No solution for day " <> show day
    Just (Solution {parser, part1, part2}) -> do
      putStrLn $ "Day " <> show day <> ":"

      (x, parseTime) <- bench (runParser parser) input
      printf "  Parser took %s\n" (showNominalDiffTime parseTime)

      (result1, time1) <- bench (force . part1) x
      printf "  Part 1 (took %s): %s\n" (showNominalDiffTime time1) (show result1)

      (result2, time2) <- bench (force . part2) x
      printf "  Part 2 (took %s): %s\n" (showNominalDiffTime time2) (show result2)

      printf "  Total time: %s\n" (showNominalDiffTime (parseTime + time1 + time2))

solveToday :: IO ()
solveToday = today >>= solve
