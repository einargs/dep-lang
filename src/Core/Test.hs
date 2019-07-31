{- @ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--exact-data-cons" @-}
{-@ LIQUID "--ple" @-}

{-# LANGUAGE TypeFamilies #-}
module Core.Test () where

import qualified Unbound.Generics.LocallyNameless as U
--import Unbound.Generics.LocallyNameless (Embed(..))
import Unbound.Generics.LocallyNameless (Bind(..))
import Unbound.Generics.LocallyNameless.Bind (Bind(B))
import Control.Lens.Iso
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

{-@ data Bind p t = B { pField :: p, tField :: t } @-}

{-@ newtype U.Embed a = Embed a @-}

--data MyEmbed a = MyEmbed a

type Var a = U.Name (SeqA a)

{- This needs the lens library -}
  {-
instance U.IsEmbed (MyEmbed t) where
  type Embedded (MyEmbed t) = t
  embedded = iso (\(MyEmbed v) -> v) (MyEmbed) -}


{-@ autosize SeqA @-}
{-@ data SeqA a = ConsA { tl1 :: (U.Embed (SeqB a)), tl2 :: SeqB a } 
                | Flag
                | NilA @-}
data SeqA a = ConsA (U.Embed (SeqB a)) (SeqB a) | Flag | NilA

{-@ autosize SeqB @-}
{-@ data SeqB a = ConsB { valB :: a, tlB :: Bind (Var a) (SeqA a) } 
                | NilB @-}
data SeqB a = ConsB a (Bind (Var a) (SeqA a)) | NilB

{-@ measure hasFlag @-}
hasFlag :: SeqA a -> Bool
hasFlag (ConsA (U.Embed tl1) tl2) = hasFlag' tl1 || hasFlag' tl2
hasFlag Flag = True
hasFlag NilA = False

{-@ measure hasFlag' @-}
hasFlag' :: SeqB a -> Bool
hasFlag' (ConsB _ (B _ tl)) = hasFlag tl
hasFlag' NilB = False

{-@ type NoFlag a = {v:SeqA a | not (hasFlag v) } @-} 
