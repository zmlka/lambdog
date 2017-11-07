module StatusComment where

import Prelude

import ShouldMerge (NegFeedback(..), PosFeedback(..))

-- What we actually want:
--
--
-- As this pull request is to the BRANCH_NAME branch, it must go through an approval process prior to being merged in. The following approvals are needed, as defined by the [configuration](link_to_config_file):
--
-- <<List of approvals >>
--
-- Approvers can give their approval by commenting below with the string:
-- `/approve`
-- A merge can be cancelled or restarted by commenting below with the strings:
-- `/nomerge` or `/domerge`


mergeMessage :: Array PosFeedback -> String
mergeMessage ps =
  "mergeMessage:\n" <>
  map posFb ps

stillNeedMessage :: Array NegFeedback -> String
stillNeedMessage ns =
  "stillNeedMessage:\n" <>
  map negFb ns

negFb :: NegFeedback -> String
negFb fb =
  case fb of
    NeedNumber {groupName, number} -> "- ✘ - Misisng at least " <> show number <>
                                      " more approval from [" <> show groupName <> "](link_to_approvers.yaml).\n"
    NeedUsers {groupName, users} -> "- ✘ - Missing necesary approvals from group [" <> show groupName <>
                                    "](link_to_approvers.yaml). Specifically:" <> show users <> ".\n"


posFb :: PosFeedback -> String
posFb fb =
  -- ideally we would have links to the approve comments also
  "- ✔ - Approvals for group [" <> show fb.groupName <>
  "](link_to_approvers.yaml) satisfied based on approves from: " <> show fb.users <> "."


-- mergeMessage [(PosFeedback {groupName: "something", condition : AtLeast 1, users : ["James", "Martin"]})]

