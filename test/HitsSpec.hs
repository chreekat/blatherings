{-# LANGUAGE OverloadedStrings #-}

module HitsSpec (tests) where

import Data.Time.Calendar (fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import Blatherings.Hits

tests :: TestTree
tests = testGroup "Blatherings.Hits"
    [ testGroup "parseEntry"
        [ testCase "decodes a counted view" $
            parseEntry "{\"time\":\"2026-10-09T14:37:25+00:00\",\"path\":\"%2Fpamir%2F\",\"referrer\":\"https%3A%2F%2Fnews.ycombinator.com%2F\",\"ua\":\"Mozilla/5.0\",\"status\":200}"
                @?= Right (View (Hit (fromGregorian 2026 10 9) "/pamir/" "https://news.ycombinator.com/"))
        , testCase "recognizes a refused request" $
            parseEntry "{\"time\":\"2026-10-09T14:37:25+00:00\",\"path\":\"/x/\",\"referrer\":\"\",\"ua\":\"curl\",\"status\":503}"
                @?= Right Refused
        , testCase "rejects a malformed line" $
            assertBool "expected Left" (either (const True) (const False) (parseEntry "{\"time\":"))
        ]
    , testGroup "referrerSource"
        [ testCase "names the referring host" $
            referrerSource "https://news.ycombinator.com/item?id=1" @?= "news.ycombinator.com"
        , testCase "marks a visit with no referrer" $
            referrerSource "" @?= "(none)"
        ]
    , testGroup "report"
        [ testCase "counts views by day, page and referrer" $
            let day = fromGregorian 2026 10
                hits =
                    [ Hit (day 10) "/a/" ""
                    , Hit (day 9) "/b/" "https://news.ycombinator.com/"
                    , Hit (day 10) "/b/" "https://news.ycombinator.com/item?id=2"
                    ]
            in report hits @?= Report
                { byDay = [(day 9, 1), (day 10, 2)]
                , byPage = [("/b/", 2), ("/a/", 1)]
                , byReferrer = [("news.ycombinator.com", 2), ("(none)", 1)]
                }
        ]
    ]
