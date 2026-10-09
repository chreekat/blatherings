module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Blatherings

main :: IO ()
main = defaultMain $ testGroup "Blatherings"
    [ testGroup "postPath"
        [ testCase "drops the directory and date prefix" $
            postPath "posts/2024-09-14-vim-fold-haskell-imports.md"
                @?= Right "vim-fold-haskell-imports/index.html"
        , testCase "treats drafts like posts" $
            postPath "drafts/2026-10-09-hello.md" @?= Right "hello/index.html"
        , testCase "rejects a file name without a date" $
            assertBool "expected Left" (isLeft (postPath "posts/hello.md"))
        , testCase "rejects a file name with only a date" $
            assertBool "expected Left" (isLeft (postPath "posts/2026-10-09.md"))
        ]
    , testGroup "tagSlug"
        [ testCase "lowercases" $ tagSlug "Haskell" @?= "haskell"
        , testCase "hyphenates runs of other characters" $
            tagSlug "bicycle  touring/Asia" @?= "bicycle-touring-asia"
        ]
    , testGroup "prettyUrl"
        [ testCase "strips a trailing index.html" $
            prettyUrl "/vim/index.html" @?= "/vim/"
        , testCase "keeps other files" $
            prettyUrl "/atom.xml" @?= "/atom.xml"
        , testCase "keeps foreign urls" $
            prettyUrl "https://example.com/index.html"
                @?= "https://example.com/index.html"
        ]
    , testGroup "absoluteUrl"
        [ testCase "prefixes root-relative urls" $
            absoluteUrl "https://words.chreekat.net" "/vim/"
                @?= "https://words.chreekat.net/vim/"
        , testCase "keeps protocol-relative urls" $
            absoluteUrl "https://words.chreekat.net" "//example.com/"
                @?= "//example.com/"
        , testCase "keeps fragments" $
            absoluteUrl "https://words.chreekat.net" "#fn1" @?= "#fn1"
        ]
    , testGroup "fingerprint"
        [ testCase "is a short hex prefix of the content's SHA-256" $
            fingerprint "abc" @?= "ba7816bf8f01"
        , testCase "hashes the UTF-8 encoding" $
            fingerprint "é" @?= "4a99557e4033"
        ]
    ]
  where
    isLeft = either (const True) (const False)
