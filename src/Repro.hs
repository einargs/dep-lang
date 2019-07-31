{-@ LIQUID "--exact-data-cons" @-}

module Repro () where

{- @ newtype Embed a = Embed a @-}
data Embed a = Embed a

{-@ autosize Binder @-}
{-@ data Binder 
  = BPi { piTy :: (Embed LTT) }
  | Guess @-}
data Binder = BPi (Embed LTT) | Guess

-- | The core dependent calculus. Based on Idris' TT.

{-@ autosize LTT @-}
{-@ data LTT = Bind { binder :: Binder, binding :: LTT } @-}
data LTT = Bind Binder LTT

{-@ measure isLttDev @-}
isLttDev :: LTT -> Bool
isLttDev (Bind (BPi (Embed t1)) t2) = isLttDev t2 || isLttDev t1
isLttDev (Bind Guess _) = True
