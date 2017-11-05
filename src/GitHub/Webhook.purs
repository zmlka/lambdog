module GitHub.Webhook where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readArray, readInt, readString, toForeign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (traverse)
import GitHub.Api (Comment, issuesGetComments, pullRequestsMerge, readComments)
import Util (decodeBase64, err)

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

decodeIssueComment :: Foreign -> F PrEvent
decodeIssueComment f = do
  _ <- f ! "comment"
  _ <- f ! "action" >>= readString -- TODO: filter actions we are interested in.
  o <- f ! "repository" ! "owner" ! "login" >>= readString
  r <- f ! "repository" ! "name" >>= readString
  n <- f ! "issue"      ! "number" >>= readInt
  pure $ PrEvent { owner: o, repo: r, number: n, event: NewPrComment }

decodeNewPr :: Foreign -> F PrEvent
decodeNewPr f = do
  _ <- f ! "pull_request"
  _ <- f ! "action" >>= readString -- TODO: filter actions we are interested in.
  n <- f ! "pull_request" ! "number" >>= readInt
  o  <- f ! "repository" ! "owner" ! "login" >>= readString
  r   <- f ! "repository" ! "name" >>= readString
  pure $ PrEvent { owner: o, repo: r, number: n, event: NewPr }

instance decodePrEvent :: Decode PrEvent where
  decode f = (decodeNewPr f) <|> (decodeIssueComment f)
