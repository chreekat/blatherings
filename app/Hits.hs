module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either (partitionEithers)
import System.IO (hPutStrLn, stderr)

import Blatherings.Hits

-- | Reads hits.chreekat.net log lines on stdin and prints readership counts.
main :: IO ()
main = do
    (errors, entries) <- partitionEithers . map parseEntry . filter (not . BL.null) . BL.lines
        <$> BL.getContents
    let views = [h | View h <- entries]
        refused = length [() | Refused <- entries]
    putStr (renderReport (report views))
    hPutStrLn stderr $
        show (length views) <> " views, " <> show refused <> " refused, "
            <> show (length errors) <> " unreadable lines"
    mapM_ (hPutStrLn stderr) (take 3 errors)
