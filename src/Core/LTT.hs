{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures,
  PatternSynonyms, DeriveAnyClass, DataKinds, TypeFamilies,
  FunctionalDependencies, StandaloneDeriving, TypeFamilyDependencies #-}
module Core.LTT (
  Var, Decl(..), DevBinder(BPi, BLam), CoreBinder(BPi, BLam),
  LTT(.., Pi, Lam), viewPi, viewLam
  ) where

import Protolude

-- Standard imports
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Fix (Fix)
import Control.Monad.Trans.Maybe

-- Library imports
import qualified Unbound.Generics.LocallyNameless as U
import Unbound.Generics.LocallyNameless (Embed(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

-- | Variable name representing an LTT term.
type Var (v :: Variant) = U.Name (LTT v)

-- | Variants of the base LTT calculus
data Variant = Core -- ^ The base LTT calculus before final typechecking
             | Dev -- ^ The development calculus adds holes and guesses

data family Binder (v :: Variant)

instance 

data instance Binder 'Core
  = CoreCanon (CanonBinder 'Core)
  deriving (Show, Generic, Typeable, U.Alpha)

data instance Binder 'Dev
  = DevCanon (CanonBinder 'Dev)
  | DevHole
  | DevGuess (Embed (LTT 'Dev))
  deriving (Show, Generic, Typeable, U.Alpha)


data CanonBinder (v :: Variant)
  = CanonPi (Embed (LTT v))
  | CanonLam
  deriving (Show, Generic, Typeable, U.Alpha)

class (Show b, Generic b, Typeable b, U.Alpha b, b ~ Binder v)
      => BinderForm (b :: *) (v :: Variant) | v -> b where
  asCanonBinder :: b -> Maybe (CanonBinder v)
  fromCanonBinder :: CanonBinder v -> b

instance BinderForm (Binder 'Core) 'Core where
  asCanonBinder (CoreCanon canon) = Just canon

  fromCanonBinder = CoreCanon

instance BinderForm (Binder 'Dev) 'Dev where
  asCanonBinder (DevCanon canon) = Just canon
  asCanonBinder _ = Nothing

  fromCanonBinder = DevCanon

pattern BPi :: LTT v -> Binder v
pattern BPi ty <- (asCanonBinder -> Just (CanonPi (Embed ty))) where
  BPi ty = fromCanonBinder (CanonPi (Embed ty))

pattern BLam :: Binder v
pattern BLam <- (asCanonBinder -> Just CanonLam) where
  BLam = fromCanonBinder CanonLam

class Variants (v :: Variant)
instance Variants 'Dev
instance Variants 'Core

--instance U.Alpha (LTT 'Dev)
--instance U.Alpha (LTT 'Core)

instance U.Subst (LTT 'Dev) (LTT 'Dev) where
  isvar (Var x) = Just (U.SubstName x)
  isvar _       = Nothing

-- | The core dependent calculus. Based on Idris' TT.
data LTT (v :: Variant)
  = Var (Var v)
  | Universe Int
  | Bind (Binder v) (U.Bind (LTT v) (LTT v))
  | App (LTT v) (LTT v)
  deriving (Show, Generic, Typeable, U.Alpha)

--deriving instance Show (Binder v) => Show (LTT v)

pattern Pi :: BinderForm b
           => LTT b
           -> U.Bind (Var b) (LTT b)
           -> LTT b
pattern Pi tyA binding = Bind (BPi tyA) binding

pattern Lam :: BinderForm b => U.Bind (Var b) (LTT b) -> LTT b
pattern Lam binding = Bind BLam binding

viewPi :: (BinderForm b, U.Fresh m)
       => LTT b -> m (Maybe (Var b, LTT b, LTT b))
viewPi (Pi tyA bnd) = do
  (v, tyB) <- U.unbind bnd
  return $ Just (v, tyA, tyB)
viewPi _ = return Nothing

viewLam :: (BinderForm b, U.Fresh m)
        => LTT b -> m (Maybe (LTT b, LTT b))
viewLam (Lam bnd) = do
  (var, body) <- U.unbind bnd
  return $ Just (var, body)
viewLam _ = return Nothing

data Decl (v :: Variant)
  = Function { term :: LTT v
             , ty :: LTT v }
