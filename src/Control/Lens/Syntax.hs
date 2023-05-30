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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Lens.Syntax
  ( over
  , traverseOf
  ) where

import Prelude hiding (exp)

import Data.List (foldl')
import Language.Haskell.TH
import qualified Control.Lens as Lens


-- >> patternToTraversal (Just (_, a))
-- traversal (\f s -> case s of
--             Just (x, a) -> (\b -> Just (x, b)) <$> f a
--             _ -> pure s)
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
patternToTraversal :: Pat -> ExpQ
patternToTraversal patWithUnderscores
  = [| Lens.traversal $ \f s -> $(do
         go patWithUnderscores >>= \case
           ([(a,b)], patWithNames, body) -> do
             let expA = VarE a
             let patB = VarP b
             [| case s of
                   $(pure patWithNames) -> (\ $(pure patB) -> $(pure body)) <$> f $(pure expA)
                   _ -> pure s
               |]
           _ -> do
             fail "lens-syntax: every pattern must bind exactly one variable") |]
  where
    -- >> go (Just (_, _))
    -- ([], Just (x, y), Just (x, y))
    -- >> go (Just (_, a))
    -- ([(a,b)], Just (x, a), Just (x, b))
    go :: Pat -> Q ([(Name, Name)], Pat, Exp)
    go = \case
      LitP lit -> do
        -- { 5 or 'c' }
        pure ([], LitP lit, LitE lit)
      VarP a -> do
        -- { a }
        b <- newName "b"
        pure ([(a,b)], VarP a, VarE b)
      TupP pats -> do
        -- { (p1,p2) }
        varMapAndPatAndExps' <- traverse go pats
        let varMap' = Lens.foldOf (Lens.each . Lens._1) varMapAndPatAndExps'
        let pats' = fmap (Lens.view Lens._2) varMapAndPatAndExps'
        let exps' = fmap (Lens.view Lens._3) varMapAndPatAndExps'
        pure (varMap', TupP pats', TupE (fmap Just exps'))
      UnboxedTupP pats -> do
        -- { (# p1,p2 #) }
        varMapAndPatAndExps' <- traverse go pats
        let varMap' = Lens.foldOf (Lens.each . Lens._1) varMapAndPatAndExps'
        let pats' = fmap (Lens.view Lens._2) varMapAndPatAndExps'
        let exps' = fmap (Lens.view Lens._3) varMapAndPatAndExps'
        pure (varMap', UnboxedTupP pats', UnboxedTupE (fmap Just exps'))
      UnboxedSumP pat sumAlt sumArity -> do
        -- { (#|p|#) }
        (varMap', pat', exp') <- go pat
        pure ( varMap'
             , UnboxedSumP pat' sumAlt sumArity
             , UnboxedSumE exp' sumAlt sumArity
             )
#if MIN_VERSION_template_haskell(2,18,0)
      ConP name types pats -> do
        -- data T1 = C1 t1 t2; {C1 @ty1 p1 p2} = e
        varMapAndPatAndExps' <- traverse go pats
        let varMaps' = Lens.toListOf (Lens.each . Lens._1) varMapAndPatAndExps'
        let pats' = fmap (Lens.view Lens._2) varMapAndPatAndExps'
        let exps' = fmap (Lens.view Lens._3) varMapAndPatAndExps'
        pure (concat varMaps', ConP name types pats', foldl' AppE (ConE name) exps')
#else
      ConP name pats -> do
        -- data T1 = C1 t1 t2; {C1 p1 p2} = e
        varMapAndPatAndExps' <- traverse go pats
        let varMap' = Lens.foldOf (Lens.each . Lens._1) varMapAndPatAndExps'
        let pats' = fmap (Lens.view Lens._2) varMapAndPatAndExps'
        let exps' = fmap (Lens.view Lens._3) varMapAndPatAndExps'
        pure (varMap', ConP name pats', foldl' AppE (ConE name) exps')
#endif
      InfixP pat1 name pat2 -> do
        -- foo ({x :+: y})
        (varMap1, pat1', exp1') <- go pat1
        (varMap2, pat2', exp2') <- go pat2
        pure ( varMap1 ++ varMap2
             , InfixP pat1' name pat2'
             , InfixE (Just exp1') (ConE name) (Just exp2')
             )
      UInfixP pat1 name pat2 -> do
        -- foo ({x :+: y})
        (varMap1, pat1', exp1') <- go pat1
        (varMap2, pat2', exp2') <- go pat2
        pure ( varMap1 ++ varMap2
             , UInfixP pat1' name pat2'
             , UInfixE exp1' (ConE name) exp2'
             )
      ParensP pat -> do
        -- { (p) }
        (varMap', pat', exp') <- go pat
        pure (varMap', ParensP pat', exp')
      TildeP pat -> do
        -- { ~p }
        (varMap', pat', exp') <- go pat
        pure (varMap', TildeP pat', exp')
      BangP pat -> do
        -- { !p }
        (varMap', pat', exp') <- go pat
        pure (varMap', BangP pat', exp')
      AsP name pat -> do
        -- { x@p }
        -- TODO: this name should be saved as part of the index
        (varMap', pat', exp') <- go pat
        pure (varMap', AsP name pat', exp')
      WildP -> do
        -- { _ }
        -- replace with a named variable so we can reconstruct the value
        name <- newName "x"
        pure ([], VarP name, VarE name)
      RecP name fieldPats -> do
        -- { Point { pointx = x, pointy = y } }
        r :: [(Name, ([(Name, Name)], Pat, Exp))]
          <- Lens.traverseOf (Lens.each . Lens._2) go fieldPats
        let varMap' = Lens.foldOf (Lens.each . Lens._2 . Lens._1) r
        let fieldPats' = Lens.over (Lens.each . Lens._2) (Lens.view Lens._2) r
        let fieldExps' = Lens.over (Lens.each . Lens._2) (Lens.view Lens._3) r
        pure (varMap', RecP name fieldPats', RecConE name fieldExps')
      ListP pats -> do
        -- { [p1,p2] }
        varMapAndPatAndExps' <- traverse go pats
        let varMap' = Lens.foldOf (Lens.each . Lens._1) varMapAndPatAndExps'
        let pats' = fmap (Lens.view Lens._2) varMapAndPatAndExps'
        let exps' = fmap (Lens.view Lens._3) varMapAndPatAndExps'
        pure (varMap', ListP pats', ListE exps')
      SigP pat typ -> do
        -- { (p :: t) }
        (varMap', pat', exp') <- go pat
        pure (varMap', SigP pat' typ, SigE exp' typ)
      ViewP {} -> do
        -- { (e -> p) }
        fail "lens-syntax: view patterns are not allowed because it is not possible to invert the function in order to get the other direction of the lens. Try to express the function as a Lens?"

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
                  setter <- [| $(pure setter0) . $(patternToTraversal pat0) . $(pure setter1Z) |]
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
