module Main where

import Prelude

import Control.Monad.Aff (Aff, catchError, error, launchAff_, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import GitHub.Api (issuesGetComments, pullRequestsMerge, readComments)
import Serverless.Request (body)
import Serverless.Response (send, setStatus)
import Serverless.Types (EXPRESS, ExpressM, Request, Response)
import ShouldMerge (getRepoConfig, shouldMerge)
import GitHub.Webhook

pullReqComments :: forall eff. PR -> Aff eff (Array { user :: String, commentText :: String })
pullReqComments (PR pr) = do
  let prReq = toForeign pr
  comments <- issuesGetComments prReq
  let _cs = readComments comments
  case _cs of
    Left err -> throwError (error "Error decoding comment.")
    Right cs -> pure cs

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = EffConsole.log "hello"

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

newtype MyReq = MyReq { name :: String }
derive instance genericMyReq :: Generic MyReq _

instance decodeMyReq :: Decode MyReq where
  decode = genericDecode opts

newtype PR = PR { owner :: String
                , repo :: String
                , number :: Int
                }

derive instance genericPR :: Generic PR _

instance decodePR :: Decode PR where
  decode = genericDecode opts

badRequest :: forall e. Response -> String -> Aff (express :: EXPRESS | e) Unit
badRequest res err = do
  setStatus res 400
  send res (toForeign { success: false, error: err })

wowza :: forall e. Request -> Response -> Aff (console :: CONSOLE, express :: EXPRESS | e) Unit
wowza req res = do
    PrEvent ev <- body req
    log (show (PrEvent ev))
    if ev.action == "created"
       then do let pr = {owner: ev.owner, repo: ev.repo, number: ev.number }
               cs <- pullReqComments (PR pr)
               config <- getRepoConfig { owner: ev.owner
                                       , targetRepo: ev.repo
                                       , configRepo: ev.repo
                                       , targetBranch: "master"
                                       , configBranch: "master" }
               let mergeThatShit = shouldMerge cs config
               _ <- if mergeThatShit
                      then do _ <- pullRequestsMerge (toForeign pr)
                              pure unit
                      else pure unit
               setStatus res 200
               send res (toForeign { success: true, comments: cs, config: config, merged: mergeThatShit })
       else badRequest res "not comment created event."
  `catchError` \err -> do log "There was an error:"
                          log (show err)
                          badRequest res (show err)

wowzaEff :: forall e. Request -> Response -> ExpressM (console :: CONSOLE | e) Unit
wowzaEff req res = launchAff_ (wowza req res)
