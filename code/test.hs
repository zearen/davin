-- This is a place holder module until cabal-test-hunit actually exists

module Main where

import System.Exit (exitFailure)

import Test.HUnit

import qualified Davɪn.Njojsɪþ.Test
import qualified Davɪn.Njojsɪþ.Latin.Test

tests :: Test
tests = test
  [ "Davin" ~: 
    [ "Njojsɪþ" ~: 
      [ "Test" ~: Davɪn.Njojsɪþ.Test.tests
      , "Latin" ~:
        [ "Test" ~: Davɪn.Njojsɪþ.Latin.Test.tests
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
