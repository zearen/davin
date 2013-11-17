{-|
This module is meant to be imported qualified, and is hence supplied
separately from "Davɪn.Njojsɪþ".  It contains all the primitive letters in
shorthand.
-}
module Conlang.Davɪn.Njojsɪþ.Letters
    ( p, b, f, v
    , t, d, s, z
    , þ, ð, ʃ, ʒ
    , k, g, x, ɣ
    , r, m, n, ŋ
    , w, j, l, h
    , y, ɪ, i, a, e, o, u
    ) where

import Conlang.Davɪn.Njojsɪþ

fromRoot :: Root -> [Consonant]
fromRoot root = zipWith (\r (v, f) -> mkCon r v f) (repeat root) 
    -- Note the order gets switched here so all plosives come before fricatives.
    $ [(v, f) | f <- bools, v <- bools]
  where bools = [False, True]

[p, b, f, v] = fromRoot C_p
[t, d, s, z] = fromRoot C_t
[þ, ð, ʃ, ʒ] = fromRoot C_þ
[k, g, x, ɣ] = fromRoot C_k
[r, m, n, ŋ] = fromRoot C_r

[w, j, l, h] = [S_w, S_j, S_l, S_h]

[y, ɪ, i, a, e, o, u] = [V_y, V_ɪ, V_i, V_a, V_e, V_o, V_u]
