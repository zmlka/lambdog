module Main where

import GitHub.Api
import Prelude

import Control.Monad.Aff (Aff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Debug.Trace (traceAny, traceShow)

bla = do
  f <- getFollowers "zmlka"
  _ <- traceAny f (\_ -> pure unit)
  log "hello"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = launchAff_ bla
