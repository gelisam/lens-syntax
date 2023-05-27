{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.Trans.State
import Data.Char (toUpper)
import Data.Foldable (sequenceA_)
import Test.DocTest

import Control.Lens.Syntax as Syntax


main :: IO ()
main = doctest ["-isrc", "test/Main.hs"]


data Person = Person
    { _name :: String
    , _age :: Int
    } deriving (Eq, Show)
makeLenses ''Person

_Identity :: Iso (Identity a) (Identity b) a b
_Identity = iso runIdentity Identity

example1 :: (Int, Identity Person)
example1 = (1, Identity (Person "Alice" 30))

example2 :: [Maybe (Int, String)]
example2 = [Just (1, "abc"), Nothing, Just (2, "def")]


-- | For 'view', don't use this library, use let bindings.
--
-- >>> personName
-- "Alice"
personName :: String
personName =
  let identityPerson = example1 ^. _2
      person = identityPerson ^. _Identity
  in person ^. name

-- | For 'preview', don't use this library, use the 'Maybe' monad.
--
-- >>> firstString
-- Just "abc"
firstString :: Maybe String
firstString = do
  maybePair <- example2 ^? each
  pair <- maybePair ^? _Just
  pair ^? _2

-- | For 'toListOf', don't use this library, use list comprehensions.
--
-- >>> strings
-- ["abc","def"]
strings :: [String]
strings =
  [ s
  | maybePair <- example2 ^.. each
  , pair <- maybePair ^.. _Just
  , s <- pair ^.. _2
  ]

-- | For 'over', this library has your back!
--
-- >>> upperStrings
-- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
upperStrings :: [Maybe (Int, String)]
upperStrings
    = $(Syntax.over [| [ toUpper c
                       | maybePair <- each example2
                       , pair <- _Just maybePair
                       , s <- _2 pair
                       , c <- each s
                       ] |])


-- | For 'traverseOf', this library has your back!
--
-- >>> runState rotateInts 0
-- ([Just (0,"abc"),Nothing,Just (1,"def")],2)
rotateInts :: State Int [Maybe (Int, String)]
rotateInts
    = $(Syntax.traverseOf [| [ do prev <- get
                                  put this
                                  pure prev
                             | maybePair <- each example2
                             , pair <- _Just maybePair
                             , this <- _1 pair
                             ] |])

-- | For 'sequenceOf_', don't use this library, use 'sequenceA_' and list
-- comprehensions.
--
-- >>> printStrings
-- abc
-- def
printStrings :: IO ()
printStrings
    = sequenceA_ [ putStrLn s
                 | maybePair <- example2 ^.. each
                 , pair <- maybePair ^.. _Just
                 , s <- pair ^.. _2
                 ]