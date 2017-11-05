module Main where

import Prelude

import Debug.Trace
import Control.Monad.Aff (Aff, catchError, error, launchAff_, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import GitHub.Api (Comment(..), issuesCreateComment, issuesEditComment, issuesGetComments, pullRequestsMerge, readComments)
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

statusComment :: forall e. PR -> Maybe Comment -> String -> Aff e Unit
statusComment pr Nothing body = trace ("status comment: " <> body) \_ -> do
  _ <- issuesCreateComment
         (toForeign { owner: pr.owner
                    , repo: pr.repo
                    , number: pr.number
                    , body: body
                    })
  pure unit
statusComment pr (Just (Comment c)) body = trace ("status comment: " <> body) \_ ->
  if c.commentText == body
     then pure unit
     else do _ <- issuesEditComment
                    (toForeign { owner: pr.owner
                               , repo: pr.repo
                               , id: c.id
                               , body: body
                               })
             pure unit

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
    let feedback = shouldMerge cs config
    let stat = getFirstLambdogComment cs
    case feedback of
      Left ns -> do statusComment pr stat (":dog: WOOF. I still need stuff:\n\n" <> show ns)
                    setStatus res 200
                    send res (toForeign { success: true, merged: false })
      Right ps -> do _ <- pullRequestsMerge (toForeign pr)
                     statusComment pr stat (":dog: WOOF. I merged!:\n\n" <> show ps)
                     setStatus res 200
                     send res (toForeign { success: true, merged: true })
  `catchError` \err -> do log "There was an error:"
                          log (show err)
                          badRequest res (show err)

wowzaEff :: forall e. Request -> Response -> ExpressM (console :: CONSOLE | e) Unit
wowzaEff req res = launchAff_ (wowza req res)

-- | Fake main
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = EffConsole.log "hello"
