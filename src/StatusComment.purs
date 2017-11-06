module StatusComment where

import Prelude
import ShouldMerge (NegFeedback, PosFeedback(..))

mergeMessage :: Array PosFeedback -> String
mergeMessage ps = "I merged! Here is why:\n\n" <> show ps

stillNeedMessage :: Array NegFeedback -> String
stillNeedMessage ns = "I still need stuff:\n\n" <> show ns
