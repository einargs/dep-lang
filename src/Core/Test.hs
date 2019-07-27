module Core.Test () where

-- import Unbound.Generics.LocallyNameless (Embed(..))



data Embed a = Embed a

{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--exact-data-cons" @-}

{-@ autosize SeqA @-}
{-@ data SeqA a = ConsA { tl1 :: Embed (SeqB a), tl2 :: SeqB a } 
                | Flag
                | NilA @-}
data SeqA a = ConsA (Embed (SeqB a)) (SeqB a) | NilA | Flag

{-@ autosize SeqB @-}
{-@ data SeqB a = ConsB { valB :: a, tlB :: SeqA a } 
                | NilB @-}
data SeqB a = ConsB a (SeqA a) | NilB

{-@ measure emFlag @-}
emFlag :: Embed (SeqB a) -> Bool
emFlag (Embed v) = hasFlag' v

{-@ measure hasFlag @-}
hasFlag :: SeqA a -> Bool
hasFlag (ConsA (Embed tl1) tl2) = hasFlag' tl1 || hasFlag' tl2
hasFlag Flag = True
hasFlag NilA = False

{-@ measure hasFlag' @-}
hasFlag' :: SeqB a -> Bool
hasFlag' (ConsB _ tl) = hasFlag tl
hasFlag' NilB = False

{-@ type NoFlag a = {v:SeqA a | not (hasFlag v) } @-} 
