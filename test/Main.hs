{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Control.Lens
import Control.Monad.Trans.State
import Data.Char (toUpper)
import Data.Foldable (sequenceA_)
import Test.DocTest

import qualified Control.Lens.Syntax as Syntax


main :: IO ()
main = doctest ["-isrc", "test/Main.hs"]


-----------------
-- Sample data --
-----------------

data Person = Person
    { _name :: String
    , _age :: Int
    } deriving (Eq, Show)
makeLenses ''Person

example1 :: (Int, Identity Person)
example1 = (42, Identity (Person "Alice" 30))

example2 :: [Maybe (Int, String)]
example2 = [Just (42, "abc"), Nothing, Just (43, "def")]


-------------------
-- Sample optics --
-------------------

_Identity :: Iso (Identity a) (Identity b) a b
_Identity = iso runIdentity Identity

iSnd :: forall x a b. IndexedLens x (x, a) (x, b) a b
iSnd = ilens get set
  where
    get :: (x, a) -> (x, a)
    get = id

    set :: (x, a) -> b -> (x, b)
    set (x, _) b = (x, b)

iName :: IndexedLens' Int Person String
iName = ilens get set
  where
    get :: Person -> (Int, String)
    get (Person name age) = (age, name)

    set :: Person -> String -> Person
    set person name = person { _name = name }


--------------------------
-- Non-indexed examples --
--------------------------

-- | For 'view', don't use this library, use let bindings.
--
-- >>> view (_2 . _Identity . name) example1
-- "Alice"
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
-- >>> preview (each . _Just . _2) example2
-- Just "abc"
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
-- >>> toListOf (each . _Just . _2) example2
-- ["abc","def"]
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
-- >>> over (each . _Just . _2 . each) toUpper example2
-- [Just (42,"ABC"),Nothing,Just (43,"DEF")]
--
-- >>> upperStrings
-- [Just (42,"ABC"),Nothing,Just (43,"DEF")]
upperStrings :: [Maybe (Int, String)]
upperStrings
  = $(Syntax.over [| [ toUpper c
                      | Just (_, s) <- each example2
                      , c <- each s
                      ] |])

-- | For 'traverseOf', this library has your back!
--
-- >>> flip runState 0 $ traverseOf (each . _Just . _1) rotateS example2
-- ([Just (0,"abc"),Nothing,Just (42,"def")],43)
--
-- >>> flip runState 0 $ rotateInts
-- ([Just (0,"abc"),Nothing,Just (42,"def")],43)
rotateInts :: State Int [Maybe (Int, String)]
rotateInts
  = $(Syntax.traverseOf [| [ rotateS this
                            | maybePair <- each example2
                            , pair <- _Just maybePair
                            , this <- _1 pair
                            ] |])

rotateS :: Int -> State Int Int
rotateS this = do
  prev <- get
  put this
  pure prev


-- | For 'traverseOf_', don't use this library, use 'sequenceA_' and list
-- comprehensions.
--
-- >>> traverseOf_ (folded . _Just . _2) putStrLn example2
-- abc
-- def
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


----------------------
-- Indexed examples --
----------------------

-- | For 'iview', don't use this library, use let bindings.
--
-- >>> iview (iSnd <.> (_Identity .> iName)) example1
-- ((42,30),"Alice")
--
-- >>> iPersonName
-- (42,30,"Alice")
iPersonName :: (Int, Int, String)
iPersonName =
  let (int, identityPerson) = example1 ^@. iSnd
      person = identityPerson ^. _Identity
      (age, name) = person ^@. iName
  in (int, age, name)

-- | For 'ipreview', don't use this library, use the 'Maybe' monad.
--
-- >>> ipreview (itraversed <.> (_Just .> iSnd)) example2
-- Just ((0,42),"abc")
--
-- >>> iFirstString
-- Just (0,42,"abc")
iFirstString :: Maybe (Int, Int, String)
iFirstString = do
  (i, maybePair) <- example2 ^@? itraversed
  pair <- maybePair ^? _Just
  (n, s) <- pair ^@? iSnd
  pure (i, n, s)

-- | For 'itoListOf', don't use this library, use list comprehensions.
--
-- >>> itoListOf (itraversed <.> (_Just .> iSnd)) example2
-- [((0,42),"abc"),((2,43),"def")]
--
-- >>> iStrings
-- [(0,42,"abc"),(2,43,"def")]
iStrings :: [(Int, Int, String)]
iStrings =
  [ (i, n, s)
  | (i, maybePair) <- example2 ^@.. itraversed
  , pair <- maybePair ^.. _Just
  , (n, s) <- pair ^@.. iSnd
  ]

-- | For 'iover', this library will have your back soon!
--
-- >>> :{
-- iover (itraversed <.> (_Just .> iSnd))
--       (\(i,n) s -> addIndices i n s)
--       example2
-- :}
-- [Just (42,"0-42-abc"),Nothing,Just (43,"2-43-def")]
--
-- >> iUpperStrings
-- [Just (42,"0-42-abc"),Nothing,Just (43,"2-43-def")]
--iTaggedStrings :: [Maybe (Int, String)]
--iTaggedStrings
--  = $(Syntax.iover [| [ addIndices i n s)
--                      | (i, maybePair) <- itraversed example2
--                      , pair <- _Just maybePair
--                      , (n, s) <- iSnd pair
--                      , s <- id s  -- clarify which variable iSnd focuses on
--                      ] |])

addIndices :: Int -> Int -> String -> String
addIndices i n s
  = show i ++ "-" ++ show n ++ "-" ++ s

-- | For 'itraverseOf', this library will have your back soon!
--
-- >>> :{
-- flip runState (0,0) $ do
--   itraverseOf (itraversed <.> (_Just .> iSnd))
--               (\(i,n) s -> addPrevIndices i n s)
--               example2
-- :}
-- ([Just (42,"0-0-abc"),Nothing,Just (43,"0-42-def")],(2,43))
--
-- >> flip runState (0,0) $ addPrevIndicesEverywhere
-- ([Just (42,"0-0-abc"),Nothing,Just (43,"0-42-def")],(2,43))
--addPrevIndicesEverywhere :: State (Int,Int) [Maybe (Int, String)]
--addPrevIndicesEverywhere
--  = $(Syntax.itraverseOf [| [ addPrevIndices i n s
--                            | (i, maybePair) <- itraversed example2
--                            , pair <- _Just maybePair
--                            , (n, s) <- iSnd pair
--                            , s <- id s  -- clarify which variable iSnd focuses on
--                            ] |])

addPrevIndices :: Int -> Int -> String -> State (Int,Int) String
addPrevIndices thisI thisN s = do
  (prevI, prevN) <- get
  put (thisI, thisN)
  pure (show prevI ++ "-" ++ show prevN ++ "-" ++ s)

-- | For 'itraverseOf_', don't use this library, use 'sequenceA_' and list
-- comprehensions.
--
-- >>> itraverseOf_ (itraversed <.> (_Just .> iSnd)) (\(i,n) s -> putStrLn (addIndices i n s)) example2
-- 0-42-abc
-- 2-43-def
--
-- >>> printTaggedStrings
-- 0-42-abc
-- 2-43-def
printTaggedStrings :: IO ()
printTaggedStrings
    = sequenceA_ [ putStrLn (addIndices i n s)
                 | (i, maybePair) <- example2 ^@.. itraversed
                 , pair <- maybePair ^.. _Just
                 , (n, s) <- pair ^@.. iSnd
                 ]
