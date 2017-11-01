module Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty as NEL

foreign import decodeBase64 :: String -> String

foo :: Int -> Int -> Int
foo x y = 2 * x + y

--

err :: forall m a. MonadThrow (NEL.NonEmptyList ForeignError) m => String -> m a
err e = throwError $ NEL.singleton (ForeignError e)
