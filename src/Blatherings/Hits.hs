{-# LANGUAGE OverloadedStrings #-}

-- | Readership counts from the hits.chreekat.net log.
module Blatherings.Hits
    ( Hit (..)
    , Entry (..)
    , parseEntry
    , referrerSource
    , Report (..)
    , report
    , renderReport
    ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Time.Calendar (Day, showGregorian)
import Network.URI (parseURI, unEscapeString, uriAuthority, uriRegName)
import Text.Printf (printf)

import Blatherings (parseDay)

-- | One page view.
data Hit = Hit
    { hitDay :: Day
    , hitPath :: String
    , hitReferrer :: String -- ^ Empty when the reader arrived directly.
    }
    deriving (Eq, Show)

-- | One log line.
data Entry
    = View Hit
    | Refused -- ^ Turned away by rate limiting.
    deriving (Eq, Show)

-- | A log line as nginx writes it, still percent-encoded.
data LogLine = LogLine
    { logTime :: String
    , logPath :: String
    , logReferrer :: String
    , logStatus :: Int
    }

instance FromJSON LogLine where
    parseJSON = withObject "hit" $ \o -> LogLine
        <$> o .: "time"
        <*> o .: "path"
        <*> o .: "referrer"
        <*> o .: "status"

parseEntry :: BL.ByteString -> Either String Entry
parseEntry line = do
    l <- eitherDecode line
    case logStatus l of
        200 -> do
            day <- parseDay (take 10 (logTime l))
            pure (View (Hit day (unEscapeString (logPath l)) (unEscapeString (logReferrer l))))
        _ -> pure Refused

-- | Where a reader came from, as a host name.
referrerSource :: String -> String
referrerSource "" = "(none)"
referrerSource r = maybe r uriRegName (uriAuthority =<< parseURI r)

data Report = Report
    { byDay :: [(Day, Int)] -- ^ Chronological.
    , byPage :: [(String, Int)] -- ^ Most viewed first.
    , byReferrer :: [(String, Int)] -- ^ Most viewed first.
    }
    deriving (Eq, Show)

report :: [Hit] -> Report
report hits = Report
    { byDay = Map.toAscList (counts hitDay)
    , byPage = mostFirst (counts hitPath)
    , byReferrer = mostFirst (counts (referrerSource . hitReferrer))
    }
  where
    counts :: Ord k => (Hit -> k) -> Map.Map k Int
    counts key = Map.fromListWith (+) [(key h, 1) | h <- hits]
    mostFirst = sortOn (Down . snd) . Map.toAscList

renderReport :: Report -> String
renderReport r = unlines $
    section "Views by day" [(showGregorian d, n) | (d, n) <- byDay r]
        <> [""]
        <> section "Views by page" (byPage r)
        <> [""]
        <> section "Views by referrer" (byReferrer r)
  where
    section title rows = title : [printf "%8d  %s" n k | (k, n) <- rows]
