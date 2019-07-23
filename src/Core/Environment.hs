module Core.Environment (
  emptyEnv, Env(..), TcMonad, runTcMonad, lookupDef, err, Err
  ) where

import Protolude

import Core.LTT
import Core.PrettyPrint

import Data.Map as Map

import qualified Unbound.Generics.LocallyNameless as U

data Env = Env { context :: Map VarName Decl }

emptyEnv :: Env
emptyEnv = Env Map.empty

data Err = Err Doc deriving (Show)

instance Disp Err where
  disp (Err doc) = doc

instance Semigroup Err where
  (Err d1) <> (Err d2) = Err (d1 <> d2)

instance Monoid Err where
  mempty = Err mempty

type TC = U.FreshMT (ReaderT Env (Except Err))

newtype TcMonad a = TcMonad { unTcMonad :: TC a }
  deriving (Functor, Applicative, Monad, U.Fresh, MonadReader Env,
            MonadError Err, Alternative, MonadPlus)

runTcMonad :: Env -> TcMonad a -> Either Err a
runTcMonad env = runExcept
               . flip runReaderT env
               . U.runFreshMT
               . unTcMonad

lookupDef :: Env -> VarName -> Maybe LTT
lookupDef env v = lookup v (context env) >>= extractExp
    where
      extractExp :: Decl -> Maybe LTT
      extractExp (Function {term}) = Just term
      extractExp _                 = Nothing

err :: (MonadError Err m) => [DispInfo] -> m b
err x = do
  throwError $ Err (disp x)
