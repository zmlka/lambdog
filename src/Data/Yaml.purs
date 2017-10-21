module Data.Yaml where

import Prelude
import Data.Foreign (F, Foreign, readArray, readString, toForeign)

foreign import _safeDump :: forall a. a -> Foreign

