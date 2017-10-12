module Main where

import GitHub.Api
import Prelude

import Control.Monad.Aff (Aff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (toForeign)
import Debug.Trace (traceAny, traceShow)

bla = do
  f <- issuesGetForRepo (toForeign {owner: "zmlka", repo: "lambdog"})
  prRevs <- pullRequestsGetReviews (toForeign {owner: "zmlka", repo: "lambdog", number: 1})
  _ <- traceAny prRevs (\_ -> pure unit)
  log "hello"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = launchAff_ bla
