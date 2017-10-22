module Data.Yaml (load, dump) where

import Prelude
import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Data.Foreign (F, Foreign, readBoolean, readString)
import Data.Foreign.Index ((!))

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

foreign import _safeLoad :: String -> Foreign

load_ :: String -> F (Either String Foreign)
load_ s = do
  let d = _safeLoad s
  success <- d ! "success" >>= readBoolean
  if success
     then do v <- d ! "value"
             pure (Right v)
     else do e <- d ! "error" >>= readString
             pure (Left e)

load :: String -> Either String Foreign
load s = case runExcept (load_ s) of
  Left e -> Left (show e)
  Right r -> r
