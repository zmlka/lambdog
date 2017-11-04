module Main where

import Prelude

import Control.Monad.Aff (Aff, catchError, error, launchAff_, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe)
import GitHub.Api (Comment(..), issuesCreateComment, issuesGetComments, pullRequestsMerge, readComments)
import GitHub.Webhook (PrEvent(..))
import Serverless.Request (body)
import Serverless.Response (send, setStatus)
import Serverless.Types (EXPRESS, ExpressM, Request, Response)
import ShouldMerge (getRepoConfig, shouldMerge)

badRequest :: forall e. Response -> String -> Aff (express :: EXPRESS | e) Unit
badRequest res err = do
  setStatus res 400
  send res (toForeign { success: false, error: err })

-- | Coordiates of a pull request.
type PR = { owner :: String
          , repo :: String
          , number :: Int
          }

-- | Get the comments for a pull request.
pullReqComments :: forall eff. PR -> Aff eff (Array Comment)
pullReqComments pr = do
  let prReq = toForeign pr
  comments <- issuesGetComments prReq
  let _cs = readComments comments
  case _cs of
    Left err -> throwError (error "Error decoding comment.")
    Right cs -> pure cs

getFirstLambdogComment :: Array Comment -> Maybe Comment
getFirstLambdogComment = find (\(Comment c) -> c.user == "lambdog")

wowza :: forall e. Request -> Response -> Aff (console :: CONSOLE, express :: EXPRESS | e) Unit
wowza req res = do
    PrEvent ev <- body req
    log (show (PrEvent ev))
    let pr = {owner: ev.owner, repo: ev.repo, number: ev.number }
    cs <- pullReqComments pr
    config <- getRepoConfig { owner: ev.owner
                            , targetRepo: ev.repo
                            , configRepo: ev.repo
                            , targetBranch: "master"
                            , configBranch: "master" }
    let mergeThatShit = shouldMerge cs config
    _ <- if mergeThatShit
         then do _ <- pullRequestsMerge (toForeign pr)
                 pure unit
         else do setStatus res 200
                 send res (toForeign { success: true, comments: cs, config: config, merged: mergeThatShit })
    let stat = getFirstLambdogComment cs
    _ <- issuesCreateComment (toForeign { owner: pr.owner, repo: pr.repo, number: pr.number, body: ":dog: WOOF. This is lambdog. I'll be managing this PR. I'm waiting for `/approve`s from ..." })
    pure unit
  `catchError` \err -> do log "There was an error:"
                          log (show err)
                          badRequest res (show err)

wowzaEff :: forall e. Request -> Response -> ExpressM (console :: CONSOLE | e) Unit
wowzaEff req res = launchAff_ (wowza req res)

-- | Fake main
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = EffConsole.log "hello"
