module Core.SpecHelp (
  varn, lam, pi, app, appL, tcPasses, tcFails,
  shouldEquate, shouldNotEquate, u0, u1, a, b,
  c, va, vb, vc, x, y, z, vx, vy, vz
  ) where

import Protolude hiding (pi)

import Core.LTT
import Core.Evaluate
import Core.PrettyPrint (disp)
import Core.Environment

import Unbound.Generics.LocallyNameless as U
import Test.Hspec

-- | Name a variable.
varn :: Text -> VarName
varn t = U.string2Name $ toS t

-- | Utility for creating lambdas.
lam :: VarName -> LTT -> LTT
lam v body = Bind (U.bind binder body)
  where binder = Lam v

-- | Utility for creating pis.
pi :: VarName -> LTT -> LTT -> LTT
pi v tyA tyB = Bind (U.bind binder tyB)
  where binder = Pi v (embed tyA)

-- | Utility for creating applications.
app :: LTT -> LTT -> LTT 
app x y = App x y

-- | Applies a list of terms as arguments to an abstraction.
appL :: LTT -> [LTT] -> LTT
appL = foldl' app

-- | Expects that the TcMonad will pass type checking.
tcPasses :: TcMonad () -> Expectation
tcPasses tc = case runTcMonad emptyEnv tc of
                Right _ -> mempty
                Left err -> expectationFailure $ show $ disp err

-- | Expects that the TcMonad will fail type checking.
tcFails :: TcMonad () -> Expectation
tcFails tc = runTcMonad emptyEnv tc `shouldSatisfy` isLeft

-- | Expects the two terms to equate.
shouldEquate :: LTT -> LTT -> Expectation
shouldEquate x y = tcPasses $ equate x y

-- | Expects the two terms not to equate.
shouldNotEquate :: LTT -> LTT -> Expectation
shouldNotEquate x y = tcFails $ equate x y

-- Convenience names.
u0, u1 :: LTT
u0 = Universe 0
u1 = Universe 1

a, b, c :: VarName
a = varn "a"
b = varn "b"
c = varn "c"

va, vb, vc :: LTT
va = Var a
vb = Var b
vc = Var c

x, y, z :: VarName
x = varn "x"
y = varn "y"
z = varn "z"

vx, vy, vz :: LTT
vx = Var x
vy = Var y
vz = Var z
