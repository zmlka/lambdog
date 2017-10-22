module GitHub.Api where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readArray, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)
import Util (decodeBase64)

readComment :: Foreign -> F { user :: String, commentText :: String }
readComment f = do
  u <- f ! "user" ! "login" >>= readString
  c <- f ! "body" >>= readString
  pure { user: u, commentText: c }

readComments_ :: Foreign -> F (Array { user :: String, commentText :: String })
readComments_ f = do
  dat <- f ! "data" >>= readArray
  ss <- traverse readComment dat
  pure ss

readComments :: Foreign -> Either String (Array { user :: String, commentText :: String })
readComments f = case runExcept (readComments_ f) of
  Left err -> Left "Couldn't parse comments JSON."
  Right ss -> Right ss

-- File Content

fileContent :: Foreign -> F String
fileContent f = do
  c <- f ! "data" ! "content" >>= readString
  pure (decodeBase64 c)

type RepoReq a = { owner :: String, repo :: String | a }

-- | Get the string contents of a text-file in a repo.
-- | `ref`: branch, e.g. "jhh/github-yaml"
-- | `path`: the path to the file, e.g. "foo/bar/config.yaml"
getFile :: forall e. RepoReq ( path :: String, ref :: String ) -> Aff e String
getFile req = do
  c' <- reposGetContent (toForeign req)
  case runExcept (fileContent c') of
    Right c -> pure c
    Left err -> throwError (error (show err))

-- | Gets a config file for the targetRepo inside the configRepo.
-- | NOTE: Assumes both repos have the same owner.
getConfigFile
  :: forall e. String
  -> { targetRepo :: String
     , configRepo :: String
     , owner :: String
     , targetBranch :: String
     , configBranch :: String }
  -> Aff e String
getConfigFile fileName r =
  getFile { owner: r.owner
          , repo: r.configRepo
          , path: "watching/" <> r.owner <> "/" <> r.targetRepo <> "/" <> r.targetBranch <> "/" <> fileName
          , ref: r.configBranch }

-- Imports

foreign import _issuesGetForRepo :: forall eff. Foreign -> EffFnAff eff Foreign
issuesGetForRepo :: forall eff. Foreign -> Aff eff Foreign
issuesGetForRepo = fromEffFnAff <<< _issuesGetForRepo

foreign import _pullRequestsGetReviews :: forall eff. Foreign -> EffFnAff eff Foreign
pullRequestsGetReviews :: forall eff. Foreign -> Aff eff Foreign
pullRequestsGetReviews = fromEffFnAff <<< _pullRequestsGetReviews

foreign import _issuesGetComments :: forall eff. Foreign -> EffFnAff eff Foreign
issuesGetComments :: forall eff. Foreign -> Aff eff Foreign
issuesGetComments = fromEffFnAff <<< _issuesGetComments

foreign import _reposGetContent :: forall eff. Foreign -> EffFnAff eff Foreign
reposGetContent :: forall eff. Foreign -> Aff eff Foreign
reposGetContent = fromEffFnAff <<< _reposGetContent

foreign import _pullRequestsMerge :: forall eff. Foreign -> EffFnAff eff Foreign
pullRequestsMerge :: forall eff. Foreign -> Aff eff Foreign
pullRequestsMerge = fromEffFnAff <<< _pullRequestsMerge
