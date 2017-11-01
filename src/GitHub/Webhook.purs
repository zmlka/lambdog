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
import Util (decodeBase64)

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

instance decodePrEvent :: Decode PrEvent where
  decode f = do
    owner  <- f ! "repository" ! "owner" ! "login" >>= readString
    repo   <- f ! "repository" ! "name" >>= readString
    number <- f ! "issue"      ! "number" >>= readInt
    action <- f ! "action" >>= readString
    e <- (f ! "comment" *> pure NewPr) <|> (f ! "pull_request" *> pure NewPrComment)
    case action of
      "created" -> pure $ PrEvent { owner: owner, repo: repo, number: number, event: e }
      _ -> throwError "Not an event we are interested in."
