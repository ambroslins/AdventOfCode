module AdventOfCode.Main (solve, today, solveToday, showNominalDiffTime) where

import AdventOfCode.Day01 qualified as Day01
import AdventOfCode.Day02 qualified as Day02
import AdventOfCode.Day03 qualified as Day03
import AdventOfCode.Prelude
import Control.DeepSeq (deepseq)
import Control.Exception (catch)
import Control.Exception.Base (throwIO)
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
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
      (3, Day03.solution)
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

bench :: (NFData a, NFData b) => (a -> b) -> a -> IO (b, NominalDiffTime)
bench f x = do
  start <- x `deepseq` getCurrentTime
  let result = f x
  end <- result `deepseq` getCurrentTime
  pure (result, diffUTCTime end start)

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime diff
  | s < 1e-5 = printf "%.3f ns" ns
  | s < 1e-4 = printf "%.2f ns" ns
  | s < 1e-3 = printf "%.1f ns" ns
  | s < 1e-2 = printf "%.3f ms" ms
  | s < 1e-1 = printf "%.2f ms" ms
  | s < 1.0 = printf "%.1f ms" ms
  | otherwise = printf "%.1f s" s
  where
    s = realToFrac diff :: Double
    ms = s * 1e3
    ns = s * 1e6

solve :: Int -> IO ()
solve day = do
  input <- readInputFile day
  case IntMap.lookup day solutions of
    Nothing -> putStrLn $ "No solution for day " <> show day
    Just (Solution {parser, part1, part2}) -> do
      putStrLn $ "Day " <> show day <> ":"
      (parseResult, parseTime) <- bench (parseOnly (parser <* endOfInput)) input
      case parseResult of
        Left err -> putStrLn $ "Parser failed on: " <> err
        Right x -> do
          printf "  Parser took %s\n" (showNominalDiffTime parseTime)
          (result1, time1) <- bench part1 x
          printf "  Part 1 (took %s): %s\n" (showNominalDiffTime time1) (show result1)
          (result2, time2) <- bench part2 x
          printf "  Part 2 (took %s): %s\n" (showNominalDiffTime time2) (show result2)
          printf "  Total time: %s\n" (showNominalDiffTime (parseTime + time1 + time2))

solveToday :: IO ()
solveToday = today >>= solve
