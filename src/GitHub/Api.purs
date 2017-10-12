module GitHub.Api where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Data.Foreign (Foreign)

foreign import _issuesGetForRepo :: forall eff. Foreign -> EffFnAff eff Foreign

issuesGetForRepo :: forall eff. Foreign -> Aff eff Foreign
issuesGetForRepo = fromEffFnAff <<< _issuesGetForRepo

foreign import _pullRequestsGetReviews :: forall eff. Foreign -> EffFnAff eff Foreign

pullRequestsGetReviews :: forall eff. Foreign -> Aff eff Foreign
pullRequestsGetReviews = fromEffFnAff <<< _pullRequestsGetReviews
