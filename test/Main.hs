{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Char
import Test.DocTest

import Control.Lens
import Control.Lens.Syntax as Syntax


main :: IO ()
main = doctest ["-isrc", "test/Main.hs"]

-- |
-- >>> maybesToUpper [Just (1, "abc"), Nothing, Just (2, "def")]
-- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
maybesToUpper :: [Maybe (Int, String)] -> [Maybe (Int, String)]
maybesToUpper maybes
    = $(Syntax.over [| [ toUpper c
                       | maybePair <- each maybes
                       , pair <- _Just maybePair
                       , s <- _2 pair
                       , c <- each s
                       ] |])
