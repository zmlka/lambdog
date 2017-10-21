module Main where

import Data.Either
import Data.Maybe
import Debug.Trace
import GitHub.Api
import IsApproval
import Prelude
import Serverless.Request
import Serverless.Response
import Serverless.Types
import Util

import Control.Monad.Aff (Aff, launchAff, launchAff_, liftEff', runAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Control.Monad.Except (runExcept)
import Data.Foreign (F, Foreign, toForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Function.Uncurried (runFn2)
import Data.Generic.Rep (class Generic)

import Data.Yaml

bla :: forall eff. PR -> Aff eff (Either String (Array String))
bla (PR pr) = do
  let prReq = toForeign pr
  comments <- issuesGetComments prReq
  let _cs = commentStrings comments
  case _cs of
    Left err -> pure (Left "error decoding comments")
    Right cs -> pure (Right cs)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = EffConsole.log "hello"

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

newtype PR = PR { owner :: String
                , repo :: String
                , number :: Int
                }

derive instance genericPR :: Generic PR _

instance decodePR :: Decode PR where
  decode = genericDecode opts

setStatus :: forall e. Response -> Int -> Aff (express :: EXPRESS | e) Unit
setStatus res n = liftEff $ runFn2 _setStatus res n

send :: forall a e. Response -> a -> Aff (express :: EXPRESS | e) Unit
send res x = liftEff $ runFn2 _send res x

wowza :: forall e. Request -> Response -> Aff (console :: CONSOLE, express :: EXPRESS | e) Unit
wowza req res = do
  f_b <- liftEff (_getBody req)
  let eb = runExcept $ decode f_b
  case eb of
    Left l -> do
      log "ERROR: badly formed PR request."
      setStatus res 400
      send res (toForeign { success: false })
    Right pr -> do
      cs_ <- bla pr
      case cs_ of
        Left err -> do
          setStatus res 400
          send res (toForeign { success: false, error: err })
        Right cs -> do
          setStatus res 200
          let ok = toForeign { success: true, comments: cs }
          send res ok

wowzaEff :: forall e. Request -> Response -> ExpressM (console :: CONSOLE | e) Unit
wowzaEff req res = launchAff_ (wowza req res)

logConfigFile :: forall e. Aff (console :: CONSOLE) Unit
logConfigFile = do
  c <- getConfigFile { owner: "zmlka"
                     , targetRepo: "lambdog"
                     , configRepo: "lambdog"
                     , targetBranch: "master"
                     , configBranch: "jhh/github-yaml-file" }
  log "config file:"
  log c
  pure unit

logConf :: forall e. Eff (console :: CONSOLE) Unit
logConf = launchAff_ logConfigFile
