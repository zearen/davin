module Util
    ( (.:)
    , (??)
    , (><)
    ) where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = ((.).(.))
infixr 9 .:

(??) :: a -> a -> Bool -> a
(left ?? right) bool = if bool then left else right
-- We want it tighter than (==).
infix 5 ??

(><) :: a -> b -> (a, b)
a >< b = (a, b)
-- It should contain expressions, but be able to be applied on.  If you're
-- mixing monads and tuples, you're up to strangeness anyway.
infix 1 ><
