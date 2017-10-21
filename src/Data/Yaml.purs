module Data.Yaml where

import Prelude

import Data.Foreign (F, Foreign, readArray, readBoolean, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Either
import Control.Monad.Except (runExcept)

foreign import _safeDump :: forall a. a -> Foreign

dump_ :: forall a. a -> F (Either String String)
dump_ x = do
  let d = _safeDump x
  success <- d ! "success" >>= readBoolean
  if success
     then do v <- d ! "value" >>= readString
             pure (Right v)
     else do v <- d ! "error" >>= readString
             pure (Left v)

dump :: forall a. a -> Either String String
dump x = case runExcept (dump_ x) of
  Left l -> Left (show l)
  Right r -> r


