-- This is a place holder module until cabal-test-hunit actually exists

module Main where

import System.Exit (exitFailure)

import Test.HUnit

import qualified Conlang.Davɪn.Njojsɪþ.Test
import qualified Conlang.Davɪn.Njojsɪþ.Latin.Test
import qualified Conlang.Davɪn.Njojsɪþ.Parsing.Test

tests :: Test
tests = test
  [ "Davin" ~: 
    [ "Njojsɪþ" ~: 
      [ "Test" ~: Conlang.Davɪn.Njojsɪþ.Test.tests
      , "Latin" ~:
        [ "Test" ~: Conlang.Davɪn.Njojsɪþ.Latin.Test.tests
        ]
      , "Parsing" ~:
        [ "Test" ~: Conlang.Davɪn.Njojsɪþ.Parsing.Test.tests
        ]
      ]
    ]
  ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts > 0 || failures counts > 0
      then exitFailure
      else return ()
