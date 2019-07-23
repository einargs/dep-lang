{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Core.PrettyPrint (
  Doc, Disp, disp, DispInfo(..), arrow
  ) where

import Protolude hiding (list)

import Core.LTT

import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Unbound.Generics.LocallyNameless as U

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

instance Disp VarName where
  disp name = str <> num
    where str = text $ toS $ U.name2String name
          intComp = U.name2Integer name
          num = if intComp == 0 then mempty else integer intComp

instance Disp LTT where
  disp (Var v) = disp v
  disp (Universe level) = text "Type" <> int level
  disp (Pi ty bnd) = panic "hey"
  disp (Bind abs bnd) = U.runFreshM $ do
    (var, body) <- U.unbind bnd
    case abs of
      BPi ty ->
        return $ parens (disp var <> colon <+> disp ty)
                  <+> arrow <+> disp body
      BLam ->
        return $ backslash <> disp var <> dot <+> disp body
  disp (App t1 t2) = parens (disp t1) <+> parens (disp t2)
