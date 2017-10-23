module GitHub.Webhook where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readArray, readInt, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Foreign.Class (class Decode)
import Data.Traversable (traverse)
import Util (decodeBase64)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype PrEvent = PrEvent { owner :: String
                          , repo :: String
                          , number :: Int
                          , action :: String
                          }

derive instance genericPrEvent :: Generic PrEvent _
instance showPrEvent :: Show PrEvent where show = genericShow

{-

{
  "action": "created",
  ...
  "issue": {
    ...
    "id": 73464126,
    "number": 2,
    "title": "Spelling error in the README file",
    "user": {
      "login": "baxterthehacker",
      "id": 6752317,
      ...
      "type": "User",
      "site_admin": false
    },
    ...
    "state": "open",
    "locked": false,
    "assignee": null,
    "body": "It looks like you accidently spelled 'commit' with two 't's."
  },
  "comment": {
    ...
    "id": 99262140,
    "user": {
      "login": "baxterthehacker",
      "id": 6752317,
      ...
      "type": "User",
      "site_admin": false
    },
    "created_at": "2015-05-05T23:40:28Z",
    "updated_at": "2015-05-05T23:40:28Z",
    "body": "You are totally right! I'll get this fixed right away."
  },
  "repository": {
    "id": 35129377,
    "name": "public-repo",
    "full_name": "baxterthehacker/public-repo",
    "owner": {
      "login": "baxterthehacker",
      "id": 6752317,
      ...
      "type": "User",
      "site_admin": false
    },
    "description": "",
    ...
    "created_at": "2015-05-05T23:40:12Z",
    "updated_at": "2015-05-05T23:40:12Z",
    "pushed_at": "2015-05-05T23:40:27Z",
    ...
    "homepage": null,
    "size": 0,
    "stargazers_count": 0,
    "watchers_count": 0,
    "language": null,
    "has_issues": true,
    "has_downloads": true,
    "has_wiki": true,
    "has_pages": true,
    "forks_count": 0,
    "mirror_url": null,
    "open_issues_count": 2,
    "forks": 0,
    "open_issues": 2,
    "watchers": 0,
    "default_branch": "master"
  },
  "sender": {
    "login": "baxterthehacker",
    "id": 6752317,
    ...
    "type": "User",
    "site_admin": false
  }
}

-}

instance decodePrEvent :: Decode PrEvent where
  decode f = do
    owner  <- f ! "repository" ! "owner" ! "login" >>= readString
    repo   <- f ! "repository" ! "name" >>= readString
    number <- f ! "issue"      ! "number" >>= readInt
    action <- f ! "action" >>= readString
    pure $ PrEvent { owner: owner, repo: repo, number: number, action: action }
