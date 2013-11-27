{-# LANGUAGE FlexibleContexts #-}

module Conlang.Davɪn.Njojsɪþ.Parsing.Internal where

import           Data.Functor.Identity
import           Data.List (foldl')
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Text.Parsec

import           Conlang.Davɪn.Njojsɪþ
import           Util

showNjojsɪþ :: (Letter -> ShowS) -> Njojsɪþ -> String
showNjojsɪþ = (($![]) . foldl' (.) id) .: map

justLookup :: Ord k => Map.Map k v -> k -> v
justLookup = flip $ fromJust .: Map.lookup

-- | Inverts a map so that keys become values and values become keys.  It's
-- expected only to be used with bijective maps, (surjectiveness is trivial,
-- injectiveness is for you to show).
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

