-- | This is an encoding of a Njojsɪþ character into raw bits.  This is useful
-- for transmission over the wire.  Also, the letters are encoded in such a
-- way that they represent a correct "alphabetic" ordering as bytes.
--
-- _ _ _ _'_ _ _ _'_ _ _ _'_ _ _ _
-- ↑ \   / \   / ↑ \   / ↑ ↑ \   /
-- u stem sprout n root  v f unused
module Conlang.Davɪn.Njojsɪþ.Bits
    (
    ) where

import Conlang.Davɪn.Njojsɪþ.Bits.Internal
