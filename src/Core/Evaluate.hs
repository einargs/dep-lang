{-# LANGUAGE 
  NoImplicitPrelude,
  GeneralizedNewtypeDeriving,
  DeriveDataTypeable,
  DeriveGeneric,
  FlexibleContexts,
  NamedFieldPuns,
  OverloadedStrings,
  ViewPatterns #-}

module Core.Evaluate (
  equate, whnf
  ) where

import Protolude

import Core.LTT
import Core.Environment
import Core.PrettyPrint

-- Library imports
import qualified Unbound.Generics.LocallyNameless as U

equate :: (U.Fresh m, MonadPlus m, MonadError Err m, MonadReader Env m)
       => LTT -> LTT -> m ()
equate term1 term2
  | U.aeq term1 term2 = return ()
  | otherwise = do
    n1 <- whnf term1
    n2 <- whnf term2
    let notEqual = typeError n1 n2
    case (n1, n2) of
      -- Universes should be equal if the terms are the same
      (Universe k, Universe j) | k == j -> return ()

      -- Variables are the same if the names are the same
      (Var x, Var y) | x == y -> return ()

      -- Lambdas are the same if the bodies are the same.
      (LPi tyA1 bnd1, LPi tyA2 bnd2) -> do
        (_, tyB1, _, tyB2) <- U.unbind2Plus bnd1 bnd2
        equate tyA1 tyA2
        equate tyB1 tyB2

      -- Pis are the same if the types are the same and the
      -- bodies are the same.
      -- 
      -- >>> (x:A) -> B = (y:A) -> B
      (LLam bnd1, LLam bnd2) -> do
        (_, t1, _, t2) <- U.unbind2Plus bnd1 bnd2
        equate t1 t2

      {-
      (Bind bnd1, Bind bnd2) -> do
        (p1, t1, p2, t2) <- U.unbind2Plus bnd1 bnd2
        case (p1, p2) of
          -- Lambdas are the same if the bodies are the same.
          (Lam {}, Lam {}) -> equate t1 t2
          -- Pis are the same if the types are the same and the
          -- bodies are the same.
          -- 
          -- >>> (x:A) -> B = (y:A) -> B
          (Pi {varType=ty1}, Pi {varType=ty2}) -> do
            equate (U.unembed ty1) (U.unembed ty2)
            equate t1 t2

          -- If none of those match, things are not equal
          _ -> notEqual
      -}
      
      -- Applications are equal if their components are equal.
      (App a1 a2, App b1 b2) -> do
        equate a1 b1
        equate a2 b2

      -- This is for handling recursive definitions, which we don't
      -- want to do until we have totality checking.
      --(Var x, _) -> recEquate x n2
      --(_, Var x) -> recEquate x n1

      -- If none of those are successful, things are not equal
      _ -> notEqual
  where
    typeError :: (MonadError Err m) => LTT -> LTT -> m ()
    typeError n1 n2 = do
      err [DS "type error: could not equate", DD n1, DS "with", DD n2]

-- | Assert that the given type is a `Pi` type (or can be normalized
-- to such).
--
-- Throws an error if this is not the case.
ensurePi :: (U.Fresh m, MonadReader Env m, MonadError Err m)
         => LTT -> m (Var, LTT, LTT)
ensurePi ty = do
  nf <- whnf ty
  case nf of
    LPi tyA bnd -> do
      (v, tyB) <- U.unbind bnd
      return (v, tyA, tyB)
    _ -> err [DS "expected function type", DD nf]

{-@ measure isWhnf @-}
isWhnf :: LTT -> Bool
isWhnf (App (Bind Lam _) _) = False
isWhnf (Var _) = True
isWhnf (App t1 _) = isWhnf t1
isWhnf _ = True

{-@ type Whnf = { v:LTT | isWhnf v } @-}

-- | Put a term into weak-head normal form.
{-@ whnf :: (U.Fresh m, MonadReader Env m) => LTT -> m Whnf @-}
whnf :: (U.Fresh m, MonadReader Env m) => LTT -> m LTT
whnf (Var x) = do
  env <- ask
  case lookupDef env x of
    Just d -> whnf d
    _      -> return $ Var x

whnf app@(App t1 t2) = do
  nf <- whnf t1
  case nf of
    LLam binding -> do
      (v, body) <- U.unbind binding
      whnf $ U.subst v t2 body
    _ -> return $ app

-- All other forms are already in whnf
whnf term = return term
