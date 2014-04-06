module Conlang.Davɪn.Njojsɪþ.Latin.Test where

import           Conlang.Davɪn.Njojsɪþ.Latin.Internal

import qualified Data.ByteString.UTF8 as UTF8
import           Test.HUnit

import           Conlang.Davɪn.Njojsɪþ
import           Conlang.Davɪn.Njojsɪþ.Letters
import           Test.Util

toLatin' = toLatin . UTF8.fromString

tests :: Test
tests = test
  [ "show" ~:
    [ fromNjojsɪþ [] ~?= 
        toLatin' ""
    , fromNjojsɪþ [loneCon l, Syl $ root t $ stem e, Special '!'] ~?= 
        toLatin' "let!"
    , "LoneCon" ~:
      [ fromNjojsɪþ [loneCon ɣ] ~?= 
          toLatin' "ɣ"
      , fromNjojsɪþ [loneCon h] ~?= 
          toLatin' "h"
      , fromNjojsɪþ [loneCon n, loneCon j] ~?= 
          toLatin' "nj"
      ]
    , "Syl" ~:
      [ fromNjojsɪþ [Syl $ stem e] ~?= 
          toLatin' "e"
      , fromNjojsɪþ [Syl $ sprout j $ stem o] ~?= 
          toLatin' "oj"
      , fromNjojsɪþ [Syl $ root ð $ stem y] ~?= 
          toLatin' "yð"
      , fromNjojsɪþ [Syl $ root f $ sprout w $ stem a] ~?= 
          toLatin' "awf"
      , fromNjojsɪþ [Syl $ nasal $ root b $ stem o] ~?= 
          toLatin' "omb"
      , fromNjojsɪþ [Syl $ nasal $ root x $ sprout j $ stem a] ~?= 
          toLatin' "ajŋx"
      , fromNjojsɪþ [Syl $ root ʒ $ stem ɪ, Syl $ stem i] ~?= 
          toLatin' "ɪʒi"
      ]
    , "Special" ~:
      [ fromNjojsɪþ [Special ' '] ~?=
          toLatin' " "
      , fromNjojsɪþ [Special '1', Special ','] ~?=
          toLatin' "1,"
      ]
    ]
  , "parse" ~:
    [ assertParse pLatin "" []
    , assertParse pLatin "wit davɪn es elnʃɪɣ ."
        [ loneCon w, Syl $ root t $ stem i, Special ' '
        , loneCon d, Syl $ root v $ stem a, Syl $ root n $ stem ɪ, Special ' '
        , Syl $ root s $ stem e, Special ' '
        , Syl $ nasal $ root ʃ $ sprout l $ stem e, Syl $ root ɣ $ stem ɪ
        , Special ' ', Special '.'
        ]
    , assertParse pLatin "anre" [Syl $ root n $ stem a, loneCon r, Syl $ stem e]
    , "pLoneCon" ~:
      [ assertParseFail pLoneCon ""
      , assertParse pLoneCon "l" $ loneCon l
      , assertParse pLoneCon "r" $ loneCon r
      , assertParseFail pLoneCon "y"
      , assertParseFail pLoneCon " "
      ]
    , "pSyllable" ~:
      [ assertParseFail pSyllable ""
      , assertParse pSyllable "i" $ Syl $ stem i
      , assertParse pSyllable "aj" $ Syl $ sprout j $ stem a
      , assertParse pSyllable "ɪð" $ Syl $ root ð $ stem ɪ
      , assertParse pSyllable "omp" $
          Syl $ nasal $ root p $ stem o
      , assertParse pSyllable "enz" $
          Syl $ nasal $ root z $ stem e
      , assertParse pSyllable "unʒ" $
          Syl $ nasal $ root ʒ $ stem u
      , assertParse pSyllable "ilŋɣ" $
          Syl $ nasal $ root ɣ $ sprout l $ stem i
      , assertParseFail pSyllable "."
      , assertParseFail pSyllable "je"
      , assertParseFail pSyllable "ter"
      ]
    , "pSpecial" ~:
      [ assertParseFail pSpecial ""
      , assertParse pSpecial " " $ Special ' '
      , assertParse pSpecial "." $ Special '.'
      , assertParse pSpecial "a" $ Special 'a'
      ]
    ]
  ]
