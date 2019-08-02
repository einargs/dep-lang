{-@ LIQUID "--exact-data-cons" @-}

module Repro2 () where

-- If you convert `newtype Embed` to `data Embed` the error goes away.
{-@ newtype Embed a = Embed a @-}
newtype Embed a = Embed a

{-@ autosize LTT @-}
{-@ data LTT = Pi { piTyA :: Embed LTT, piTyB :: LTT }
             | Universe
             | Var @-}
data LTT = Pi (Embed LTT) LTT
         | Universe
         | Var

{-@ measure isLttDev @-}
isLttDev :: LTT -> Bool
isLttDev (Pi (Embed t1) t2) = isLttDev t2 || isLttDev t1
isLttDev Universe = True
isLttDev Var = False
