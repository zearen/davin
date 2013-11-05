{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

module Davɪn.Njojsɪþ
    ( Root(..) 
    , Consonant
    , mkCon
    , conRoot
    , conVoiced
    , conFricative
    , Stem(..)
    , Sprout(..)
    , Syllable
    , mkSyl
    , sylCon
    , sylStem
    , sylSprout
    , sylNasal
    , Letter(..)
    , Njojsɪþ
    , NjojsɪþEncoding
    , toNjojsɪþ
    , fromNjojsɪþ
    ) where

import Control.Lens
import Data.List

lexicographicOrder :: [Ordering] -> Ordering
lexicographicOrder [] = EQ
lexicographicOrder ords = foldl1 (\acc x -> if acc == EQ then x else acc) ords

data Root 
    = C_p
    | C_t
    | C_þ
    | C_k
    | C_r
  deriving (Show, Eq, Ord, Enum)

data Consonant = Consonant
    { _conRoot :: Root
    , _conVoiced :: Bool
    , _conFricative :: Bool
    }
  deriving (Show, Eq)

instance Ord Consonant where
    compare (Consonant r1 v1 f1) (Consonant r2 v2 f2) = lexicographicOrder
        [ compare r1 r2
        , compare v1 v2
        , compare f1 f2
        ]

makeLenses ''Consonant

mkCon :: Root -> Bool -> Bool -> Consonant
mkCon root voiced fricative = Consonant root voiced fricative

data Stem 
    = V_y
    | V_ɪ
    | V_i
    | V_a
    | V_e
    | V_o
    | V_u
  deriving (Show, Eq, Ord, Enum)

data Sprout
    = S_w
    | S_j
    | S_l
    | S_h            
  deriving (Show, Eq, Ord, Enum)

data Syllable = Syllable
    { _sylCon :: Maybe Consonant
    , _sylStem :: Stem
    , _sylSprout :: Maybe Sprout
    , _sylNasal :: Bool
    }
  deriving (Show, Eq)

instance Ord Syllable where
    compare (Syllable c1 v1 s1 n1) (Syllable c2 v2 s2 n2)
      = lexicographicOrder
        [ compare c1 c2
        , compare v1 v2
        , compare s1 s2
        , compare n1 n2
        ]

makeLenses ''Syllable

mkSyl :: Maybe Consonant -> Stem -> Maybe Sprout -> Bool -> Syllable
mkSyl con stem sprout nasal = Syllable con stem sprout nasal

data Letter
    = LoneCon Consonant
    | Syl Syllable
    | Special Char
  deriving (Show, Eq)

instance Ord Letter where
    compare (LoneCon con1) (LoneCon con2) = compare con1 con2
    compare (LoneCon _) _ = LT
    compare (Syl syl1) (Syl syl2) = compare syl1 syl2
    compare (Syl _) _ = LT
    compare (Special char1) (Special char2) = compare char1 char2

type Njojsɪþ = [Letter]

class NjojsɪþEncoding enc where
    toNjojsɪþ :: enc -> Either String Njojsɪþ
    fromNjojsɪþ :: Njojsɪþ -> enc

