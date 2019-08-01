module spec Unbound.Generics.LocallyNameless where

newtype Embed a = Embed { embbeddedValue :: a }
data Bind p t = B { pField :: p, tField :: t }
