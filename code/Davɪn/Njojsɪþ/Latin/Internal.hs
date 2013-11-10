{-# LANGUAGE FlexibleContexts #-}

module Davɪn.Njojsɪþ.Latin.Internal where

import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Text.Parsec

import           Davɪn.Njojsɪþ
import qualified Davɪn.Njojsɪþ.Letters as L
import           Util

newtype Latin = Latin B.ByteString

toLatin :: B.ByteString -> Latin
toLatin = Latin

fromLatin :: Latin -> B.ByteString
fromLatin (Latin str) = str

conTable :: Map.Map Consonant Char
conTable = Map.fromList
    [ L.p >< 'p'
    , L.b >< 'b'
    , L.f >< 'f'
    , L.v >< 'v'
    , L.t >< 't'
    , L.d >< 'd'
    , L.s >< 's'
    , L.z >< 'z'
    , L.þ >< 'þ'
    , L.ð >< 'ð'
    , L.ʃ >< 'ʃ'
    , L.ʒ >< 'ʒ'
    , L.k >< 'k'
    , L.g >< 'g'
    , L.x >< 'x'
    , L.ɣ >< 'ɣ'
    , L.r >< 'r'
    , L.m >< 'm'
    , L.n >< 'n'
    , L.ŋ >< 'ŋ'
    ]

stemTable :: Map.Map Stem Char
stemTable = Map.fromList
    [ L.y >< 'y'
    , L.ɪ >< 'ɪ'
    , L.i >< 'i'
    , L.a >< 'a'
    , L.e >< 'e'
    , L.o >< 'o'
    , L.u >< 'u'
    ]

sproutTable :: Map.Map Sprout Char
sproutTable = Map.fromList
    [ L.w >< 'w'
    , L.j >< 'j'
    , L.l >< 'l'
    , L.h >< 'h'
    ]

nasalTable :: Map.Map Root Char
nasalTable = Map.fromList
    [ C_p >< 'm'
    , C_t >< 'n'
    , C_þ >< 'n'
    , C_k >< 'ŋ'
    , C_r >< error "Davɪn.Njojsɪþ: Corrupt Syllable (nasalized C_r)"
    ]

showsLetter :: Letter -> ShowS
showsLetter (LoneCon (Left root')) = (justLookup conTable root':)
showsLetter (LoneCon (Right spr)) = (justLookup sproutTable spr:)
showsLetter (Syl syl) = stem' . sprout' . nasal' . root'
  where stem' = (justLookup stemTable (syl^.sylStem):)
        sprout' = case syl^.sylSprout of
            Nothing -> id
            Just s -> (justLookup sproutTable s:)
        root' = case syl^.sylRoot of
            Nothing -> id
            Just root' -> (justLookup conTable root':)
        nasal' = if syl^.sylNasal
                  then
                    case syl^.sylRoot of
                        Nothing -> error $ "Davɪn.Njojsɪþ: Corrupt Syllable "
                            ++ "(nasalized lone vowel)"
                        Just root' -> (justLookup nasalTable (root'^.conRoot):)
                  else id
showsLetter (Special ch) = (ch:)

showNjojsɪþ :: Njojsɪþ -> String
showNjojsɪþ = ($![]) . foldl' (.) id . map showsLetter

pLatin :: Stream s Identity Char => Parsec s () Njojsɪþ
pLatin = many pLetter

pLetter :: Stream s Identity Char => Parsec s () Letter
pLetter = pLoneCon <|> pSyllable <|> pSpecial

pLoneCon :: Stream s Identity Char => Parsec s () Letter
pLoneCon = fmap LoneCon $ 
    fmap Left (pFromTable conTable)
    <|> fmap Right (pFromTable sproutTable)

pSyllable :: Stream s Identity Char => Parsec s () Letter
pSyllable = do
    stem_ <- pFromTable stemTable
    sprout_ <- pMaybe $ pFromTable sproutTable
    (root_, nasal_) <- pNasal <|> pCon
    return $ Syl $ set sylNasal nasal_ $ mkSyl stem_ sprout_ root_
  where pNasal = try $ do
            ch <- oneOf "mnŋ"
            root' <- pFromTable conTable
            if ch == justLookup nasalTable (root'^.conRoot)
              then return (Just root', True)
              else fail "Found nasal', but didn't assimilate"
        pCon = do
            root' <- pMaybe $ pFromTable conTable
            return (root', False)
    

pSpecial :: Stream s Identity Char => Parsec s () Letter
pSpecial = fmap Special anyChar

instance NjojsɪþEncoding Latin where
    toNjojsɪþ latin = case parse pLatin "" $ fromLatin latin of
        Left err -> Left $ show err
        (Right njojsɪþ) -> Right njojsɪþ
    fromNjojsɪþ = toLatin . UTF8.fromString . showNjojsɪþ

justLookup :: Ord k => Map.Map k v -> k -> v
justLookup = flip $ fromJust .: Map.lookup

invertMap :: (Ord k, Ord v) => Map.Map k v -> Map.Map v k
invertMap = Map.fromList . map invertTuple . Map.toList
  where invertTuple (a, b) = (b, a)

pFromTable :: (Ord k, Stream s Identity Char) => Map.Map k Char -> Parsec s () k
pFromTable table = try $ do
    mbK <- flip fmap anyChar $ flip Map.lookup $ invertMap table
    case mbK of
        Nothing -> fail "Expected Just from parser"
        Just k -> return k

pMaybe :: Stream s Identity Char => Parsec s () a -> Parsec s () (Maybe a)
pMaybe p = fmap Just p <|> return Nothing

