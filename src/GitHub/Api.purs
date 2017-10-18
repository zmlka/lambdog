module GitHub.Api where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readArray, readString)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)

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

commentString :: Foreign -> F String
commentString f = do
  s <- f ! "body"
  readString s

commentStrings_ :: Foreign -> F (Array String)
commentStrings_ f = do
  dat <- f ! "data" >>= readArray
  ss <- traverse commentString dat
  pure ss

commentStrings :: Foreign -> Either String (Array String)
commentStrings f = case runExcept (commentStrings_ f) of
  Left err -> Left "Bad JSON"
  Right ss -> Right ss
