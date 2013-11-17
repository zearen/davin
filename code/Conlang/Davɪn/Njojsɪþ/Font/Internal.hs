module Conlang.Davɪn.Njojsɪþ.Font.Internal where


import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Text.Parsec

import           Conlang.Davɪn.Njojsɪþ
import qualified Conlang.Davɪn.Njojsɪþ.Letters as L
import           Util

newtype Font = Font B.ByteString

toFont :: B.ByteString -> Font
toFont = Font

fromFont :: Font -> B.ByteString
fromFont (Font str) = str
