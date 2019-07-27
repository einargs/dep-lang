{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures,
  PatternSynonyms, DeriveAnyClass, DataKinds, TypeFamilies,
  FunctionalDependencies, StandaloneDeriving, TypeFamilyDependencies,
  QuasiQuotes #-}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--exact-data-cons" @-}
module Core.LTT (
  Var, Decl(..), Binder(..),
  LTT(.., Pi, Lam), viewPi, viewLam
  ) where

import Protolude

-- Standard imports
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

-- Library imports
import qualified Unbound.Generics.LocallyNameless as U
import Unbound.Generics.LocallyNameless (Embed(..))
import qualified Unbound.Generics.LocallyNameless.Unsafe as U
import Unbound.Generics.LocallyNameless.Bind

-- | Variable name representing an LTT term.
type Var = U.Name LTT

{-@ autosize Binder @-}
{-@ data Binder 
  = BPi { piTy :: (Embed LTT) }
  | BLam
  | Hole
  | Guess { guessTy :: (Embed LTT) } @-}
data Binder
  = BPi (Embed LTT)
  | BLam
  | Hole
  | Guess (Embed LTT)
  deriving (Show, Generic, Typeable, U.Alpha)

type LttBinding = U.Bind Var LTT

-- | The core dependent calculus. Based on Idris' TT.

{-@ autosize LTT @-}
{-@ data LTT
  = Var { var :: Var }
  | Universe { universeLevel :: Nat }
  | Bind { binder :: Binder, binding :: U.Bind Var LTT }
  | App { appR :: LTT, appL :: LTT } @-}
data LTT
  = Var Var
  | Universe Int
  | Bind Binder LttBinding
  | App LTT LTT
  deriving (Show, Generic, Typeable, U.Alpha)

type Var' = U.Name LTT'
{-@ autosize LTT' @-}
{-@ data LTT'
  = Var' { var' :: Var' }
  | Universe' { universeLevel' :: Nat }
  | Pi' { piTy' :: LTT', piVar' :: Var', piBody' :: LTT' }
  | Lam' { lamVar' :: Var', lamBody' :: LTT' }
  | Hole' { holeVar' :: Var', holeBody' :: LTT' }
  | Guess' { guessTy' :: LTT', guessVar' :: Var', guessBody' :: LTT' }
  | App' { appR' :: LTT', appL' :: LTT' } @-}
data LTT'
  = Var' Var
  | Universe' Int
  | Pi' LTT' Var' LTT'
  | Lam' Var' LTT'
  | Hole' Var' LTT'
  | Guess' LTT' Var' LTT'
  | App' LTT' LTT'
  deriving (Show, Generic, Typeable, U.Alpha)

{- @ measure lttLen @-}
{- @ lttLen :: LTT' -> Nat  @-}
  {- lttLen :: LTT' -> Int
lttLen (Var' _) = 0
lttLen (Universe' _) = 0
lttLen (Bind' binder _ body) = 1 + max' (binderLen binder) (lttLen body)
lttLen (App' t1 t2) = 1 + max' (lttLen t1) (lttLen t2) -}

{-@ measure isLttDev' @-}
isLttDev' :: LTT' -> Bool
isLttDev' (Var' _) = False
isLttDev' (Universe' _) = False
isLttDev' (App' _ _) = False
isLttDev' (Pi' tyA _ tyB) = isLttDev' tyA || isLttDev' tyB
isLttDev' (Lam' _ body) = isLttDev' body
isLttDev' (Hole' _ body) = isLttDev' body 
isLttDev' (Guess' tyA _ tyB) = isLttDev' tyA || isLttDev' tyB

{-@ type LttDev = { v:LTT | isLttDev v } @-}
{-@ type LttCore = { v:LTT | not (isLttDev v) } @-}

pattern Pi :: LTT -> U.Bind Var LTT -> LTT
pattern Pi tyA binding = Bind (BPi (Embed tyA)) binding

pattern Lam :: U.Bind Var LTT -> LTT
pattern Lam binding = Bind BLam binding

instance U.Subst LTT Binder

instance U.Subst LTT LTT where
  isvar (Var x) = Just (U.SubstName x)
  isvar _       = Nothing

viewPi :: (U.Fresh m) => LTT -> m (Maybe (Var, LTT, LTT))
viewPi (Pi tyA bnd) = do
  (v, tyB) <- U.unbind bnd
  return $ Just (v, tyA, tyB)
viewPi _ = return Nothing

viewLam :: (U.Fresh m) => LTT -> m (Maybe (Var, LTT))
viewLam (Lam bnd) = do
  (var, body) <- U.unbind bnd
  return $ Just (var, body)
viewLam _ = return Nothing

data Decl
  = Function { term :: LTT
             , ty :: LTT }
