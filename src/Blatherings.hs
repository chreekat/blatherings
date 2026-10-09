-- | The pure parts of the site generator.
module Blatherings
    ( postPath
    , tagSlug
    , prettyUrl
    , absoluteUrl
    , fingerprint
    , parseDay
    , replyMailto
    ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.URI (escapeURIString, isUnreserved)
import System.FilePath (takeBaseName, (</>))

-- | Output path of a post or draft named @YYYY-MM-DD-slug.ext@: @slug/index.html@.
postPath :: FilePath -> Either String FilePath
postPath src = case splitAt 11 (takeBaseName src) of
    (date, slug@(_ : _)) | isDatePrefix date -> Right (slug </> "index.html")
    _ -> Left ("Expected a file name like 2026-10-09-some-slug.md, got " <> src)
  where
    isDatePrefix d = length d == 11 && and (zipWith matches "dddd-dd-dd-" d)
    matches 'd' c = isDigit c
    matches p c = p == c

-- | URL-safe form of a tag name.
tagSlug :: String -> String
tagSlug = hyphenate . map toLower
  where
    hyphenate s = case break (not . isAlphaNum) s of
        (word, []) -> word
        (word, rest) -> case dropWhile (not . isAlphaNum) rest of
            [] -> word
            rest' -> word <> "-" <> hyphenate rest'

-- | Local URLs without a trailing @index.html@.
prettyUrl :: String -> String
prettyUrl u
    | "/" `isPrefixOf` u, "/index.html" `isSuffixOf` u =
        take (length u - length "index.html") u
    | otherwise = u

-- | Root-relative URLs made absolute under the given site root.
absoluteUrl :: String -> String -> String
absoluteUrl root u
    | "/" `isPrefixOf` u, not ("//" `isPrefixOf` u) = root <> u
    | otherwise = u

-- | Content-derived cache-busting token.
fingerprint :: String -> String
fingerprint = take 12 . BC.unpack . convertToBase Base16 . hashWith SHA256 . encodeUtf8 . T.pack

-- | A front-matter date, @YYYY-MM-DD@.
parseDay :: String -> Either String Day
parseDay s = maybe (Left ("Expected a date like 2026-10-09, got " <> s)) Right
    (parseTimeM False defaultTimeLocale "%Y-%m-%d" s)

-- | A link that starts an email replying to the titled post.
replyMailto :: String -> String -> String
replyMailto address title =
    "mailto:" <> address <> "?subject=" <> escapeURIString isUnreserved ("Re: " <> title)
