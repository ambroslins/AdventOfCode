
{-# LANGUAGE OverloadedLists #-}
module AdventOfCode.Main (solve, today, solveToday) where

import AdventOfCode.Day01 qualified as Day01
import AdventOfCode.Prelude
import Control.Exception (catch)
import Control.Exception.Base (throwIO)
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import Data.ByteString qualified as BS
import Data.Time
  ( addUTCTime,
    getCurrentTime,
    nominalDay,
    toGregorian,
    utctDay,
  )
import Data.Vector ((!?))
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

solutions :: Vector Solution
solutions = [Day01.solution]

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
  estTime <- addUTCTime (negate (5 * 3600 + 5 * 60)) <$> getCurrentTime
  print estTime
  let (_, _, day) = toGregorian $ utctDay estTime
  pure day

solve :: Int -> IO ()
solve day = do
  input <- readInputFile day
  case solutions !? (day - 1) of
    Nothing -> putStrLn $ "No solution for day " <> show day
    Just (Solution {parser, part1, part2}) -> do
      putStrLn $ "Day " <> show day <> ":"
      case parseOnly (parser <* endOfInput) input of
        Left err -> putStrLn $ "Parser failed on: " <> err
        Right x -> do
          putStrLn $ "  Part 1: " <> show (part1 x)
          putStrLn $ "  Part 2: " <> show (part2 x)


solveToday :: IO ()
solveToday = today >>= solve
