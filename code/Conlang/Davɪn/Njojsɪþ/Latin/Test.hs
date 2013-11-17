module Conlang.Davɪn.Njojsɪþ.Latin.Test where

import Conlang.Davɪn.Njojsɪþ.Latin.Internal

import Test.HUnit

import Conlang.Davɪn.Njojsɪþ
import Conlang.Davɪn.Njojsɪþ.Letters
import Test.Util

tests :: Test
tests = test
  [ "show" ~:
    [ showNjojsɪþ [] ~?= ""
    , showNjojsɪþ [loneCon l, Syl $ root t $ stem e, Special '!'] ~?= "let!"
    , "LoneCon" ~:
      [ showNjojsɪþ [loneCon ɣ] ~?= "ɣ"
      , showNjojsɪþ [loneCon h] ~?= "h"
      , showNjojsɪþ [loneCon n, loneCon j] ~?= "nj"
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

