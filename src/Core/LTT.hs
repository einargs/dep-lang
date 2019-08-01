{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses #-}
{-@ LIQUID "--exact-data-cons" @-}

module Core.LTT (
  Binder(..), LTT(.., LPi, LLam, LHole, LGuess), Var, Decl(..),
  viewLam, viewPi
  ) where

import Protolude

-- Library imports
import qualified Unbound.Generics.LocallyNameless as U
--import Unbound.Generics.LocallyNameless (Bind(..))
import Unbound.Generics.LocallyNameless.Bind (Bind(B))
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

-- | Variable name representing an LTT term.
type Var = U.Name LTT

{-@ autosize Binder @-}
{-@ data Binder 
  = Pi { piTy :: (U.Embed LTT) }
  | Lam
  | Hole
  | Guess { guessTy :: (U.Embed LTT) } @-}
data Binder
  = Pi (U.Embed LTT)
  | Lam
  | Hole
  | Guess (U.Embed LTT)
  deriving (Show, Generic, Typeable)

-- | The core dependent calculus. Based on Idris' TT.

{-@ autosize LTT @-}
{-@ data LTT
  = Var { var :: Var }
  | Universe { universeLevel :: Nat }
  | Bind { binder :: Binder, binding :: Bind Var LTT }
  | App { appR :: LTT, appL :: LTT } @-}
data LTT
  = Var Var
  | Universe Int
  | Bind Binder (Bind Var LTT)
  | App LTT LTT
  deriving (Show, Generic, Typeable)

{-@ measure isLttDev @-}
isLttDev :: LTT -> Bool
isLttDev (Var _) = False
isLttDev (Universe _) = False
isLttDev (Bind (Pi (U.Embed t1)) (B _ t2)) = isLttDev t2 || isLttDev t1
isLttDev (Bind Lam (B _ t)) = isLttDev t
isLttDev (Bind Hole _) = True
isLttDev (Bind (Guess _) _) = True
isLttDev (App l r) = isLttDev l || isLttDev r

{-@ measure isBinderDev @-}
isBinderDev :: Binder -> Bool
isBinderDev (Pi (U.Embed t)) = isLttDev t
isBinderDev Lam = False
isBinderDev Hole = True
isBinderDev (Guess _) = True

{-@ type CoreLTT = {v: LTT | not (isLttDev v) } @-}
{-@ type DevLTT = {v: LTT | isLttDev v } @-}

pattern LPi :: LTT -> U.Bind Var LTT -> LTT
pattern LPi tyA binding = Bind (Pi (U.Embed tyA)) binding

pattern LLam :: U.Bind Var LTT -> LTT
pattern LLam binding = Bind Lam binding

pattern LHole :: U.Bind Var LTT -> LTT
pattern LHole binding = Bind Hole binding

pattern LGuess :: LTT -> U.Bind Var LTT -> LTT
pattern LGuess tyA binding = Bind (Guess (U.Embed tyA)) binding

instance U.Alpha LTT
instance U.Alpha Binder

instance U.Subst LTT Binder

instance U.Subst LTT LTT where
  isvar (Var x) = Just (U.SubstName x)
  isvar _       = Nothing

viewPi :: (U.Fresh m) => LTT -> m (Maybe (Var, LTT, LTT))
viewPi (LPi tyA bnd) = do
  (v, tyB) <- U.unbind bnd
  return $ Just (v, tyA, tyB)
viewPi _ = return Nothing

viewLam :: (U.Fresh m) => LTT -> m (Maybe (Var, LTT))
viewLam (LLam bnd) = do
  (var, body) <- U.unbind bnd
  return $ Just (var, body)
viewLam _ = return Nothing

data Decl
  = Function { term :: LTT
             , ty :: LTT }
