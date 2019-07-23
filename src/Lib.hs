module Lib
    ( someFunc
    ) where

import Protolude

someFunc :: MonadIO m => m ()
someFunc = putText "someFunc\n"
