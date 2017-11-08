module GitHub.Webhook where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (throwError)
import Data.Foreign (F, Foreign, readInt, readString)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Util (err)

-- | Pull request webhook events.
data Event
  = NewPrComment
  | NewPr

derive instance genericEvent :: Generic Event _
instance showEvent :: Show Event where show = genericShow

newtype PrEvent = PrEvent { owner :: String
                          , repo :: String
                          , number :: Int
                          , event :: Event
                          }

derive instance genericPrEvent :: Generic PrEvent _
instance showPrEvent :: Show PrEvent where show = genericShow

-- | Decode an IssueCommentEvent event.
-- | But check that it is a comment on an issue corresponding to a PR.
-- | Example payload:
-- | https://developer.github.com/v3/activity/events/types/#issuecommentevent
decodeIssueComment :: Foreign -> F PrEvent
decodeIssueComment f = do
  _ <- f ! "comment"
  _ <- f ! "action" >>= readString -- TODO: filter actions we are interested in.
  o <- f ! "repository" ! "owner" ! "login" >>= readString
  r <- f ! "repository" ! "name" >>= readString
  n <- f ! "issue"      ! "number" >>= readInt
  -- This is just to make sure this is a comment on a PR:
  _ <- f ! "issue" ! "pull_request" ! "url" >>= readString
  pure $ PrEvent { owner: o, repo: r, number: n, event: NewPrComment }

-- | Decode a PullRequestEvent.
-- | Example payload:
-- | https://developer.github.com/v3/activity/events/types/#pullrequestevent
decodeNewPr :: Foreign -> F PrEvent
decodeNewPr f = do
  _ <- f ! "pull_request"
  _ <- f ! "action" >>= readString -- TODO: filter actions we are interested in.
  n <- f ! "pull_request" ! "number" >>= readInt
  state <- f ! "pull_request" ! "state" >>= readString
  o <- f ! "repository" ! "owner" ! "login" >>= readString
  r <- f ! "repository" ! "name" >>= readString
  if state == "open"
     then pure $ PrEvent { owner: o, repo: r, number: n, event: NewPr }
     else err "Not interested in this event."

instance decodePrEvent :: Decode PrEvent where
  decode f = (decodeNewPr f) <|> (decodeIssueComment f)
