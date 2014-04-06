module Conlang.Davɪn.Njojsɪþ.Parsing.Test where

import           Conlang.Davɪn.Njojsɪþ.Parsing.Internal

import           Control.Applicative ((<|>))
import qualified Data.Map as Map
import           Text.Parsec (char)
import           Test.HUnit

import           Test.Util
import           Util

parseTable = Map.fromList [1 >< '1', 4 >< '4']

tests :: Test
tests = test
  [ "justLookup" ~:
    [ justLookup (Map.fromList ['a' >< 1, 'b' >< 2]) 'b' ~?= 2
    , assertPreludeError "Expect fromJust failure" $
        justLookup (Map.fromList ['a' >< 1, 'b' >< 2]) 'c'
    ]
  , "invertMap" ~:
    [ invertMap (Map.fromList ['a' >< 1, 'b' >< 2]) ~?=
        Map.fromList [1 >< 'a', 2 >< 'b']
    ]
  , "pFromTable" ~:
    [ assertParse (pFromTable parseTable) "1" 1
    -- Make sure we don't consume any input on failure.
    , assertParse (pFromTable parseTable <|> (char '2' >> return 3)) "2" 3
    , assertParseFail (pFromTable parseTable) "3"
    ]
  , "pMaybe" ~:
    [ assertParse (pMaybe $ char 'e') "e" $ Just 'e'
    , assertParse (pMaybe $ char 'q') "z" Nothing
    ]
  ]
