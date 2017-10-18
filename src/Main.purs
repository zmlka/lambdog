module Main where

import Data.Either
import Debug.Trace
import GitHub.Api
import IsApproval
import Prelude
import Serverless.Request
import Serverless.Response
import Serverless.Types

import Control.Monad.Aff (Aff, launchAff, launchAff_, liftEff', runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Control.Monad.Except (runExcept)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Function.Uncurried (runFn2)
import Data.Generic.Rep (class Generic)

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

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

newtype MyReq = MyReq { name :: String }
derive instance genericMyReq :: Generic MyReq _

instance decodeMyReq :: Decode MyReq where
  decode = genericDecode opts

wow :: forall e. Request -> Response -> ExpressM (console :: CONSOLE | e) Unit
wow req res = do
  b <- _getBody req
  let er = runExcept $ decode b
  case er of
    Left l -> do
      EffConsole.log "ERROR"
      pure unit
    Right (MyReq r) -> do
      EffConsole.log r.name
      runFn2 _setStatus res 200
      let ok = toForeign { success: true }
      runFn2 _send res ok
      pure unit
