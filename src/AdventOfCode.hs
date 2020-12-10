module AdventOfCode where

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
import Data.String
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Network.HTTP.Client (Cookie (..), createCookieJar)
import Network.HTTP.Req
import System.Directory
import System.FilePath

newtype Day = Day Int
  deriving (Eq, Ord)

getInput :: IsString s => Day -> IO s
getInput (Day d) = fromFile >>= maybe fromWebsite pure
  where
    file = "input" </> show d <.> "txt"
    fromFile = (Just . fromString <$> readFile file) `catch` (\e -> const (pure Nothing) (e :: IOException))
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
      BS.writeFile file $ input
      pure $ fromString $ BS.unpack input
