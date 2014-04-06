{-# LANGUAGE FlexibleContexts #-}

module Test.Util
    ( assertException
    , assertPreludeError
    , assertParse
    , assertParseFail
    ) where


import Control.Exception (catch, ErrorCall, Exception)
import Data.Functor.Identity
import Text.Parsec (parse, Parsec, Stream)

import Test.HUnit

assertException :: Exception e => String -> (e -> ()) -> IO a -> Test
assertException msg exVoid act = test $
    (act >> assertFailure msg)
    `catch`
    (return . exVoid)

assertPreludeError :: String -> a -> Test
assertPreludeError msg expr = assertException msg
    (\e -> const () (e :: ErrorCall)) 
    (return $! expr)

assertParse :: (Stream s Identity t, Eq a, Show a)
    => Parsec s () a -> s -> a -> Test
assertParse pParser input expected = test $
    case parse pParser "" input of
        Right result -> if result == expected
            then return ()
            else assertFailure $
                "expected: " ++ show expected ++
                "\n but got: " ++ show result
        Left err -> assertFailure $ "Parse fail: " ++ show err

assertParseFail :: (Stream s Identity t, Show a)
    => Parsec s () a -> s -> Test
assertParseFail pParser input = test $
    case parse pParser "" input of
        Right result -> assertFailure 
            $ "Should not parse but parsed to: \n" ++ show result
        Left _ -> return ()
