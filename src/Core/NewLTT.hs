{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--ple" @-}

{-# LANGUAGE TypeFamilies, DeriveAnyClass, DerivingVia #-}

module Core.NewLTT () where

import Protolude

-- Standard imports
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

-- Library imports
import qualified Unbound.Generics.LocallyNameless as U
--import Unbound.Generics.LocallyNameless (Bind(..))
--import Unbound.Generics.LocallyNameless.Bind (Bind(B))
import Control.Lens.Iso
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data Bind p t = B p t
  deriving (Show)

-- | Variable name representing an LTT term.
type Var = U.Name LTT

{-@ autosize Binder @-}
{-@ data Binder 
  = BPi { piTy :: (U.Embed LTT) }
  | BLam
  | Hole
  | Guess { guessTy :: (U.Embed LTT) } @-}
data Binder
  = BPi (U.Embed LTT)
  | BLam
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
isLttDev (Bind (BPi (U.Embed t1)) (B _ t2)) = isLttDev t2
isLttDev (Bind BLam (B _ t)) = isLttDev t
isLttDev (Bind Hole _) = True
isLttDev (Bind (Guess _) _) = True
isLttDev (App l r) = isLttDev l || isLttDev r

{-@ measure isBinderDev @-}
isBinderDev :: Binder -> Bool
isBinderDev (BPi (U.Embed t)) = isLttDev t
isBinderDev BLam = False
isBinderDev Hole = True
isBinderDev (Guess _) = True
