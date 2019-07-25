{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures,
  PatternSynonyms, DeriveAnyClass, DataKinds, TypeFamilies,
  FunctionalDependencies, StandaloneDeriving, TypeFamilyDependencies,
  QuasiQuotes #-}
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
import Unbound.Generics.LocallyNameless.Unsafe as U
import Unbound.Generics.LocallyNameless.Bind (Bind(B))

-- | Variable name representing an LTT term.
type Var = U.Name LTT

{-@ data Binder [binderLen]
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

{-@ measure binderLen @-}
binderLen :: Binder -> Int
binderLen (BPi (Embed ty)) = 1 + lttLen ty --embedLen ety
binderLen BLam = 0
binderLen Hole = 0
binderLen (Guess (Embed ty)) = 1 + lttLen ty
{-@ invariant {v: Binder | binderLen v >= 0} @-}

type LttBinding = U.Bind Var LTT

{-@ measure isBindingDev @-}
isBindingDev :: LttBinding -> Bool
isBindingDev (B _ t) = isLttDev t

termOfBinding :: LttBinding -> LTT
termOfBinding (B _ t) = t

  {-@ measure bindingLen :: b:LttBinding -> {v:Int | v = lttLen (termOfBinding
 t) @-}
bindingLen :: LttBinding -> Int
bindingLen (B _ t) = lttLen t
{-@ invariant {v: LttBinding | bindingLen v >= 0} @-}

-- | The core dependent calculus. Based on Idris' TT.

{-@ data LTT [lttLen]
  = Var { var :: Var }
  | Universe { universeLevel :: Int }
  | Bind { binder :: Binder, binding :: U.Bind Var LTT }
  | App { appR :: LTT, appL :: LTT } @-}
data LTT
  = Var Var
  | Universe Int
  | Bind Binder (U.Bind Var LTT)
  | App LTT LTT
  deriving (Show, Generic, Typeable, U.Alpha)

{-@ measure lttLen @-}
lttLen :: LTT -> Int
lttLen (Var _) = 0
lttLen (Universe _) = 0
lttLen (Bind binder binding) = 1 + binderLen binder + bindingLen binding
lttLen (App t1 t2) = 1 + lttLen t1 + lttLen t2

{-@ invariant {v: LTT | lttLen v >= 0} @-}

{-@ measure isBinderDev @-}
isBinderDev :: Binder -> Bool
isBinderDev (BPi (Embed ty)) = isLttDev ty --isLttDev tyA && isLttDev tyB
isBinderDev BLam = False -- isLttDev body
isBinderDev _ = True

{-@ measure isLttDev @-}
isLttDev :: LTT -> Bool
isLttDev (Var _) = False
isLttDev (Universe _) = False
isLttDev (App _ _) = False
isLttDev (Bind binder binding) = isBinderDev binder && isBindingDev binding

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
