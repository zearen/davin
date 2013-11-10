module Davɪn.Njojsɪþ.Latin.Test where

import Davɪn.Njojsɪþ.Latin.Internal

import Test.HUnit

import Davɪn.Njojsɪþ
import Davɪn.Njojsɪþ.Letters
import Test.Util ()

tests :: Test
tests = test
  [ "show" ~:
    [ showNjojsɪþ [] ~?= ""
    , showNjojsɪþ [loneSprout l, Syl $ root t $ stem e, Special '!'] ~?= "let!"
    , "LoneCon" ~:
      [ showNjojsɪþ [loneCon ɣ] ~?= "ɣ"
      , showNjojsɪþ [loneSprout h] ~?= "h"
      , showNjojsɪþ [loneCon n, loneSprout j] ~?= "nj"
      ]
    , "Syl" ~:
      [ showNjojsɪþ [Syl $ stem e] ~?= "e"
      , showNjojsɪþ [Syl $ sprout j $ stem o] ~?= "oj"
      , showNjojsɪþ [Syl $ root ð $ stem y] ~?= "yð"
      , showNjojsɪþ [Syl $ root f $ sprout w $ stem a] ~?= "awf"
      , showNjojsɪþ [Syl $ nasal $ root b $ stem o] ~?= "omb"
      , showNjojsɪþ [Syl $ nasal $ root x $ sprout j $ stem a] ~?= "ajŋx"
      , showNjojsɪþ [Syl $ root ʒ $ stem ɪ, Syl $ stem i] ~?= "ɪʒi"
      ]
    , "Special" ~:
      [ showNjojsɪþ [Special ' '] ~?= " "
      , showNjojsɪþ [Special '1', Special ','] ~?= "1,"
      ]
    ]
  ]

