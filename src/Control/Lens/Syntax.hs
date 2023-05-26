-- | An alternative syntax for compositions of lenses, setters, and traversals.
--
-- The standard way to compose lenses, using '(.)', is a point-free style that
-- can be hard to read under some circumstances. This module provides a
-- pointful alternative based on the list comprehension syntax.
--
--
-- With "Control.Lens":
--
-- > -- |
-- > -- >>> maybesToUpper [Just (1, "abc"), Nothing, Just (2, "def")]
-- > -- [Just (1,"ABC"),Nothing,Just (2,"DEF")]
-- > maybesToUpper :: [Maybe (Int, String)] -> [Maybe (Int, String)]
-- > maybesToUpper
-- >   = over (each . _Just . _2) toUpper
--
-- With "Control.Lens.Syntax":
--
-- > maybesToUpper :: [Maybe (Int, String)] -> [Maybe (Int, String)]
-- > maybesToUpper maybes
-- > = [over|
-- >     [ toUpper c
-- >     | Just (_, s) <- each maybes
-- >     , c <- each s
-- >     ] |]
--
-- Without lenses:
--
-- > maybesToUpper :: [Maybe (Int, String)] -> [Maybe (Int, String)]
-- > maybesToUpper []
-- >   = []
-- > maybesToUpper (Nothing : maybes)
-- >   = Nothing : maybesToUpper maybes
-- > maybesToUpper (Just (n, s) : maybes)
-- >   = Just (n, map toUpper s) : maybesToUpper maybes
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Lens.Syntax where

import Language.Haskell.TH
import qualified Control.Lens as Lens
import Data.Char


over :: ExpQ -> ExpQ
over listCompQ = do
  listCompQ >>= \case
    CompE stmts -> do
      (input, setter, f) <- go stmts
      [| Lens.over $(pure setter) $(pure f) $(pure input) |]
    _ -> fail "over: expected a list comprehension"
  where
    go :: [Stmt] -> Q (Exp, Exp, Exp)
    go (BindS (VarP boundName) (AppE setter0 input) : stmts) = do
      (setter1Z, f) <- goSetterF boundName stmts
      setter <- [| $(pure setter0) . $(pure setter1Z) |]
      pure (input, setter, f)
    go (_ : _) = do
      fail "over: expected each step in the list comprehension to be a (<-)"
    go [] = do
      error "impossible: CompE always ends with a NoBindS"

    goSetterF :: Name -> [Stmt] -> Q (Exp, Exp)
    goSetterF expectedInput (BindS (VarP boundName) (AppE setter1 (VarE actualInput)) : stmts)
      | expectedInput == actualInput = do
          (setter2Z, f) <- goSetterF boundName stmts
          setter <- [| $(pure setter1) . $(pure setter2Z) |]
          pure (setter, f)
    goSetterF inputVar (NoBindS fBody : []) = do
      f <- [| \ $(varP inputVar) -> $(pure fBody) |]
      setter <- [| id |]
      pure (setter, f)
    goSetterF [] = do
      error "impossible: CompE always ends with a NoBindS"