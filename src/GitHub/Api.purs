module GitHub.Api where

import Control.Monad.Aff
import Prelude
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Data.Foreign (Foreign)

foreign import _getFollowers :: forall eff. String -> EffFnAff eff Foreign

getFollowers :: forall eff. String -> Aff eff Foreign
getFollowers = fromEffFnAff <<< _getFollowers
