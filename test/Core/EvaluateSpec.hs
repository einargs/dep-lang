module Core.EvaluateSpec (spec) where

import Protolude hiding (pi)

import Core.SpecHelp
import Core.Evaluate
import Core.LTT

import Test.Hspec

-- Test code
lamId :: LTT
lamId = lam x vx

lamConst :: LTT
lamConst = lam x $ lam y vx

{- Use for later spec
-- A -> A
piIdA :: LTT
piIdA = pi x va va

-- (c:Z) -> c -> c
piPolyId :: LTT
piPolyId = pi c vz $ pi x vc vc

-- A -> B -> A
piConstAB :: LTT
piConstAB = pi x va $ pi y vb va

-- (a:Type0) -> (b:Type0) -> a -> b -> a
piPolyConst :: LTT
piPolyConst = pi a vz $ pi b u0 $ pi x va $ pi y vb va
--}

-- Spec
spec :: Spec
spec = do
  describe "equate" $ do
    context "when dealing with universes" $ do
      it "equates two equal universes" $
        u0 `shouldEquate` u0
      it "does not equate unequal universes" $
        u1 `shouldNotEquate` u0

    context "when dealing with variables" $ do
      it "equates the same variable" $ 
        Var a `shouldEquate` Var a
      it "does not equate different variables" $
        Var a `shouldNotEquate` Var b

    context "when dealing with simple lambdas" $ do
      -- Also makes sure `Env` isn't contaminating anything else
      it "equates equal lambdas" $ tcPasses $ do
        lam x va `equate` lam y va
        lam x vx `equate` lam y vy
        equate
          (lam x $ lam x $ vx)
          (lam y $ lam y $ vy)
        equate
          (lam y $ lam x $ u1)
          (lam x $ lam y $ u1)
      it "does not equate unequal lambdas" $ do
        lam x va `shouldNotEquate` lam x vb
        shouldNotEquate
          (lam x $ lam x vx)
          (lam y $ lam x vy)
    
    context "when dealing with simple pis" $ do
      it "equates equal pis" $ do
        pi x va vb `shouldEquate` pi y va vb
        pi x va vx `shouldEquate` pi y va vy
       
      it "equates nested pis" $ do
        shouldEquate
          -- (x:z) -> (y:x) -> y
          (pi x vz $ pi y vx vy)
          -- (a:z) -> (b:a) -> b
          (pi a vz $ pi b va vb)

      it "does not equate unequal pis" $ do
        shouldNotEquate (pi x va vx) (pi y vb vy)
        shouldNotEquate
          -- (x:a) -> (y:x) -> y
          (pi x va $ pi y vx vy)
          -- (a:x) -> (b:a) -> b
          (pi a vx $ pi b va vb)

    context "when dealing with application" $ do
      it "equates application of lambdas" $ do
        app lamId vy `shouldEquate` vy
        app (app lamId lamId) vy `shouldEquate` app lamId vy
        app (app lamConst vx) vy `shouldEquate` vx
        app (lam x (app vx va)) lamId `shouldEquate` va
        app lamConst vx `shouldEquate` lam y vx

      it "does not equate unequal application of lambdas" $ do
        app lamId vy `shouldNotEquate` app lamId vx
        app (app lamConst vy) vx  `shouldNotEquate` vx

      it "does not reduce application of pis" $ do
        app (pi x va va) vz `shouldEquate` app (pi y va va) vz
        shouldNotEquate
          (app (pi a u0 $ pi x va va) vz)
          (pi x vz vz)


