{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

module Davɪn.Njojsɪþ.Internal where

import Control.Lens
import Data.List

-- I set up all the Ord instances manually since it's really important that
-- they're right and I don't want to maintain (nor do I trust) generated code.
-- This is a helper function that takes what is essentially a list of compares
-- across fields and returns the first non-'EQ' or EQ.
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

-- | Sprouts roughly correspond to glides (and are sometimes refered to as
-- slides), though are more general.  They either behave like consonants alone,
-- or as diphthongs on a 'Stem' (pure vowel) in a 'Syllable'
data Sprout
    = S_w
    | S_j
    | S_l
    | S_h            
  deriving (Show, Eq, Ord, Enum)

-- | Represents a Davɪn syllable, which must have at least a 'Stem'
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

-- | A constructor for 'Syllable's
mkSyl :: Maybe Consonant -> Stem -> Maybe Sprout -> Bool -> Syllable
mkSyl con stem sprout nasal = Syllable con stem sprout nasal

data Letter
    = LoneCon (Either Consonant Sprout)
    | Syl Syllable
    | Special Char
  deriving (Show, Eq)

-- | A convenience function for creating 'Consonant' 'LoneCon's
loneCon :: Consonant -> LoneCon
loneCon = LoneCon . Left

-- | A convenience function for creating 'Sprout' 'LoneCon's
loneSprout :: Sprout -> LoneCon
loneSprout = LoneCon . Right

instance Ord Letter where
    compare (LoneCon eiCon1) (LoneCon eiCon2) = compareLoneCon eiCon1 eiCon2
    compare (LoneCon _) _ = LT
    compare (Syl syl1) (Syl syl2) = compare syl1 syl2
    compare (Syl _) _ = LT
    compare (Special char1) (Special char2) = compare char1 char2
      where compareLoneCon (Left con1) (Left con2) = compare con1 con2
            compareLoneCon (Left _) _ = LT
            compareLoneCon (Right spr1) (Right spr2) = compare spr1 spr2


type Njojsɪþ = [Letter]

class NjojsɪþEncoding enc where
    toNjojsɪþ :: enc -> Either String Njojsɪþ
    fromNjojsɪþ :: Njojsɪþ -> enc

