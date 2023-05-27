{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.Trans.State
import Data.Char
import Test.DocTest

import Control.Lens.Syntax as Syntax


main :: IO ()
main = doctest ["-isrc", "test/Main.hs"]


example :: [Maybe (Int, String)]
example = [Just (1, "abc"), Nothing, Just (2, "def")]

-- |
-- >>> upperStrings
-- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
upperStrings :: [Maybe (Int, String)]
upperStrings
    = $(Syntax.over [| [ toUpper c
                       | maybePair <- each example
                       , pair <- _Just maybePair
                       , s <- _2 pair
                       , c <- each s
                       ] |])


-- |
-- >>> runState rotateInts 0
-- ([Just (0,"abc"),Nothing,Just (1,"def")],2)
rotateInts :: State Int [Maybe (Int, String)]
rotateInts
    = $(Syntax.traverseOf [| [ do prev <- get
                                  put this
                                  pure prev
                             | maybePair <- each example
                             , pair <- _Just maybePair
                             , this <- _1 pair
                             ] |])