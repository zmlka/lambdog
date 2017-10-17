module IsApproval where

import GitHub.Api
import Prelude

import Control.Monad.Aff (Aff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (toForeign)
import Debug.Trace (traceAny, traceShow)
import Data.Array

isApproval :: String -> Boolean
isApproval "/approve" = true
isApproval _ = false

isApprovalCount :: Array String -> Int
isApprovalCount xs =
  xs
  # filter isApproval
  # length
