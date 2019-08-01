{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Core.PrettyPrint (
  Doc, Disp, disp, DispInfo(..), arrow
  ) where

import Protolude hiding (list)

import Core.LTT

import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Unbound.Generics.LocallyNameless as U
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

class Disp a where
  disp :: a -> Doc

data DispInfo = DS Text -- ^ Display string/text
              | forall a. Disp a => DD a -- ^ Display data

instance Disp DispInfo where
  disp (DS txt) = textStrict txt
  disp (DD obj) = indent 4 $ disp obj

instance Disp [DispInfo] where
  disp = vsep . fmap disp

arrow :: Doc
arrow = text "->"

instance Disp Var where
  disp name = str <> num
    where str = text $ toS $ U.name2String name
          intComp = U.name2Integer name
          num = if intComp == 0 then mempty else integer intComp

instance Disp LTT where
  disp (Var v) = disp v
  disp (Universe level) = text "Type" <> int level
  disp (App t1 t2) = parens (disp t1) <+> parens (disp t2)
  disp (Bind binder binding) = 
    let (var, body) = unsafeUnbind binding
     in case binder of
          Pi (U.Embed ty) ->
            parens (disp var <> colon <+> disp ty)
              <+> arrow <+> disp body
          Lam ->
            backslash <> disp var <> dot <+> disp body
          _ ->
            panic "I don't think these should be here"
