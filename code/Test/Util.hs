module Test.Util
    ( assertException
    , assertPreludeError
    , 
    ) where

import Control.Exception (catch, ErrorCall, Exception)

import Test.HUnit

assertException :: Exception e => String -> (e -> ()) -> IO a -> IO ()
assertException msg exVoid act =
    (act >> assertFailure msg)
    `catch`
    (return . exVoid)

assertPreludeError :: String -> a -> IO ()
assertPreludeError msg expr = assertException msg
    (\e -> const () (e :: ErrorCall)) 
    (return $! expr)
