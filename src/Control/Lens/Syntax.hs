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
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Lens.Syntax
  ( over
  , traverseOf
  ) where

import Prelude hiding (exp)

import Data.List (foldl')
import Language.Haskell.TH
import qualified Control.Lens as Lens


-- >> patternToSetter (Just (_, y))
-- setting (\a2b s -> case s of
--           Just (x, y) -> Just (x, a2b y)
--           _ -> pure s)
--
-- which is basically (_Just . _2).
--
-- We currently only support monomorphic traversals @Traversal' s a@, not
-- type-changing traversals @Traversal s t a b@. That's because a pattern like
-- @Just (_, y)@ is insufficient, we also need to know the other type
-- constructors in order to prove that the 'a' can be changed to a 'b':
--
-- > monomorphicMap :: (a -> a) -> Maybe (x, a) -> Maybe (x, a)
-- > monomorphicMap f (Just (x, y)) = Just (x, f y)
-- > monomorphicMap f s  -- s :: Maybe (x, a)
-- >                = s  -- s :: Maybe (x, a)
--
-- > typeChangingMap :: (a -> b) -> Maybe (x, a) -> Maybe (x, b)
-- > typeChangingMap f (Just (x, y)) = Just (x, f y)
-- > typeChangingMap f Nothing  -- Nothing :: Maybe (x, a)
-- >                 = Nothing  -- Nothing :: Maybe (x, b)
patternToSetter :: Pat -> ExpQ
patternToSetter patWithUnderscores
  = [| Lens.setting $ \a2b s -> $(do
         fExp <- [|a2b|]
         (patWithNames, exp) <- go fExp patWithUnderscores
         [| case s of
              $(pure patWithNames) -> $(pure exp)
              _ -> s
           |]) |]
  where
    -- Convert a pattern like
    --
    -- > \case
    -- >   Just (_, y) -> ...
    --
    -- to
    --
    -- > \case
    -- >   Just (x, y) -> Just (x, f y)
    --
    -- That is, apply the f to the named variable, and replace the underscores
    -- with temporary names so that we can preserve their values.
    go :: Exp -> Pat -> Q (Pat, Exp)
    go f = \case
      LitP lit -> do
        -- { 5 or 'c' }
        pure (LitP lit, LitE lit)
      VarP name -> do
        -- { x }
        pure (VarP name, AppE f (VarE name))
      TupP pats -> do
        -- { (p1,p2) }
        patAndExps' <- traverse (go f) pats
        let pats' = fmap fst patAndExps'
        let exps' = fmap snd patAndExps'
        pure (TupP pats', TupE (fmap Just exps'))
      UnboxedTupP pats -> do
        -- { (# p1,p2 #) }
        patAndExps' <- traverse (go f) pats
        let pats' = fmap fst patAndExps'
        let exps' = fmap snd patAndExps'
        pure (UnboxedTupP pats', UnboxedTupE (fmap Just exps'))
      UnboxedSumP pat sumAlt sumArity -> do
        -- { (#|p|#) }
        (pat', exp') <- go f pat
        pure (UnboxedSumP pat' sumAlt sumArity, UnboxedSumE exp' sumAlt sumArity)
#if MIN_VERSION_template_haskell(2,18,0)
      ConP name types pats -> do
        -- data T1 = C1 t1 t2; {C1 @ty1 p1 p2} = e
        patAndExps' <- traverse (go f) pats
        let pats' = fmap fst patAndExps'
        let exps' = fmap snd patAndExps'
        pure (ConP name types pats', foldl' AppE (ConE name) exps')
#else
      ConP name pats -> do
        -- data T1 = C1 t1 t2; {C1 p1 p2} = e
        patAndExps' <- traverse (go f) pats
        let pats' = fmap fst patAndExps'
        let exps' = fmap snd patAndExps'
        pure (ConP name pats', foldl' AppE (ConE name) exps')
#endif
      InfixP pat1 name pat2 -> do
        -- foo ({x :+ y}) = e
        (pat1', exp1') <- go f pat1
        (pat2', exp2') <- go f pat2
        pure (InfixP pat1' name pat2', InfixE (Just exp1') (ConE name) (Just exp2'))
      UInfixP pat1 name pat2 -> do
        -- foo ({x :+ y}) = e
        (pat1', exp1') <- go f pat1
        (pat2', exp2') <- go f pat2
        pure (UInfixP pat1' name pat2', UInfixE exp1' (ConE name) exp2')
      ParensP pat -> do
        -- {(p)}
        (pat', exp') <- go f pat
        pure (ParensP pat', ParensE exp')
      TildeP pat -> do
        -- { ~p }
        (pat', exp') <- go f pat
        pure (TildeP pat', exp')
      BangP pat -> do
        -- { !p }
        (pat', exp') <- go f pat
        pure (BangP pat', exp')
      AsP name pat -> do
        -- { x@p }
        -- TODO: this name should be saved as part of the index
        (pat', exp') <- go f pat
        pure (AsP name pat', exp')
      WildP -> do
        -- { _ }
        -- replace with a named variable so we can reconstruct the value
        name <- newName "x"
        pure (VarP name, VarE name)
      RecP name fieldPats -> do
        -- { Point { pointx = x, pointy = y } }
        fieldPatAndExps <- Lens.traverseOf
                             (Lens.each . Lens._2)
                             (go f)
                             fieldPats
        let fieldPats' = Lens.over (Lens.each . Lens._2) fst fieldPatAndExps
        let fieldExps' = Lens.over (Lens.each . Lens._2) snd fieldPatAndExps
        pure (RecP name fieldPats', RecConE name fieldExps')
      ListP pats -> do
        -- { [p1,p2,p3] }
        patAndExps' <- traverse (go f) pats
        let pats' = fmap fst patAndExps'
        let exps' = fmap snd patAndExps'
        pure (ListP pats', ListE exps')
      SigP pat typ -> do
        -- { p :: t }
        (pat', exp') <- go f pat
        pure (SigP pat' typ, SigE exp' typ)
      ViewP {} -> do
        -- { e -> p }
        fail "lens-syntax: view patterns are not allowed because it is not possible to invert the function in order to get the other direction of the lens. Try to express the function as a Lens?`"

patternVars :: Pat -> [Name]
patternVars = \case
  VarP name
    -> [name]
  LitP _
    -> []
  TupP pats
    -> concatMap patternVars pats
  UnboxedTupP pats
    -> concatMap patternVars pats
  UnboxedSumP pat _ _
    -> patternVars pat
#if MIN_VERSION_template_haskell(2,18,0)
  ConP _ _ pats
    -> concatMap patternVars pats
#else
  ConP _ pats
    -> concatMap patternVars pats
#endif
  InfixP pat1 _ pat2
    -> patternVars pat1 ++ patternVars pat2
  UInfixP pat1 _ pat2
    -> patternVars pat1 ++ patternVars pat2
  ParensP pat
    -> patternVars pat
  TildeP pat
    -> patternVars pat
  BangP pat
    -> patternVars pat
  AsP _ pat
    -> patternVars pat
  WildP
    -> []
  RecP _ fieldPats
    -> concatMap (patternVars . snd) fieldPats
  ListP pats
    -> concatMap patternVars pats
  SigP pat _
    -> patternVars pat
  ViewP _ pat
    -> patternVars pat

-- >> :{
-- traversalAction
--   over
--   [ toUpper c
--   | Just (_, s) <- each example2
--   , c <- each s
--   ]
-- :}
-- over (each . _Just . _2 . each) toUpper example2
traversalAction :: ExpQ -> ExpQ -> ExpQ
traversalAction actionQ listCompQ = do
  listCompQ >>= \case
    CompE stmts -> do
      (input, setter, actionArg) <- getInputAndSetterAndActionArg stmts
      [| $(actionQ) $(pure setter) $(pure actionArg) $(pure input) |]
    _ -> fail "lens-syntax: expected a list comprehension"
  where
    -- >> :{
    -- getSetterAndActionArg
    --   [ Just (_, s) <- each example2
    --   , c <- each s
    --   , toUpper c
    --   ]
    -- :}
    -- (example2, (each . _Just . _2 . each), toUpper)
    getInputAndSetterAndActionArg :: [Stmt] -> Q (Exp, Exp, Exp)
    getInputAndSetterAndActionArg (stmt : stmts) = do
      case stmt of
        BindS pat0 exp0 -> do
          case patternVars pat0 of
            [boundName] -> do
              case exp0 of
                AppE setter0 input -> do
                  (setter1Z, actionArg) <- getSetterAndActionArg boundName stmts
                  setter <- [| $(pure setter0) . $(patternToSetter pat0) . $(pure setter1Z) |]
                  pure (input, setter, actionArg)
                _ -> do
                  fail "lens-syntax: the first step in the list comprehension must be a setter applied to the structure s"
            _ -> do
              fail "lens-syntax: all the patterns in the list comprehension must bind a single variable"
        _ -> do
          fail "lens-syntax: all steps in the list comprehension must be a (<-)"
    getInputAndSetterAndActionArg [] = do
      error "impossible: CompE always ends with a NoBindS"

    -- >> :{
    -- getSetterAndActionArg
    --   "s"
    --   [ c <- each s
    --   , toUpper c
    --   ]
    -- :}
    -- (each, toUpper)
    getSetterAndActionArg :: Name -> [Stmt] -> Q (Exp, Exp)
    getSetterAndActionArg prevBoundVar (BindS pat1 exp1 : stmts) = do
      case pat1 of
        VarP boundName -> do
          case exp1 of
            AppE setter1 (VarE actualInput)
              | actualInput == prevBoundVar -> do
                  (setter2Z, actionArg) <- getSetterAndActionArg boundName stmts
                  setter <- [| $(pure setter1) . $(pure setter2Z) |]
                  pure (setter, actionArg)
            _ -> do
              fail "lens-syntax: all the steps in the list comprehension must be a setter applied to a variable"
        _ -> do
          fail "lens-syntax: all the patterns in the list comprehension must be a single variable"
    getSetterAndActionArg inputVar (NoBindS fBody : []) = do
      actionArg <- [| \ $(varP inputVar) -> $(pure fBody) |]
      setter <- [| id |]
      pure (setter, actionArg)
    getSetterAndActionArg _ (_ : _) = do
      fail "lens-syntax: all steps in the list comprehension must be a (<-)"
    getSetterAndActionArg _ [] = do
      error "impossible: CompE always ends with a NoBindS"

over :: ExpQ -> ExpQ
over = traversalAction [| Lens.over |]

traverseOf :: ExpQ -> ExpQ
traverseOf = traversalAction [| Lens.traverseOf |]
