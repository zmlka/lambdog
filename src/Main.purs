module Main where

import GitHub.Api
import Prelude
import Data.Either
import Control.Monad.Aff (Aff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (toForeign)
import Debug.Trace (traceAny, traceShow)
import IsApproval

bla :: forall eff. Int -> Aff (console :: CONSOLE | eff) Unit
bla n = do
  let pr =toForeign {owner: "zmlka", repo: "lambdog", number: n}
  comments <- issuesGetComments pr
  let _cs = commentStrings comments
  case _cs of
    Left err -> log err
    Right cs -> do
      let n = isApprovalCount cs
      traceShow cs (\_ -> pure unit)
      log "hello"

checkPR :: forall e. Int -> Eff (console :: CONSOLE | e) Unit
checkPR n = launchAff_ (bla n)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = launchAff_ $ bla 2

-- | Just to check if we can use purescript.
testy :: String -> String
testy name = "Hello " <> name <> "! This string was assembled in Purescript!"

