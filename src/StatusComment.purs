module StatusComment where

import Prelude

import Data.Foldable (fold)
import ShouldMerge (Feedback(..), NegFeedback(..), PosFeedback(..))

-- What we actually want:
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
  "Approved:\n" <> fold (map posFb ps)

stillNeedMessage :: Array Feedback -> String
stillNeedMessage ns =
  "Missing approval:\n" <> fold (map feedback ns)

feedback :: Feedback -> String
feedback fb =
  let msgBegin = "As this pull request is to the BRANCH_NAME branch, it must go through an approval process prior to being merged in. The following approvals are needed, as defined by the [configuration](link_to_config_file):\n\n"
      msgEnd = "\n Approvers can give their approval by commenting below with the string:\n `/approve`\n A merge can be cancelled or restarted by commenting below with the strings:\n `/nomerge` or `/domerge`"
      msgMiddle = case fb of
        Pos fb -> posFb fb
        Neg fb -> negFb fb
  in
   msgBegin <> msgMiddle <> msgEnd

negFb :: NegFeedback -> String
negFb fp =
  case fp of
    NeedNumber {groupName, number} -> "- ✘ - Missisng at least " <> show number <>
                                      " more approval from [" <> show groupName <> "](link_to_approvers.yaml).\n"
    NeedUsers {groupName, users} -> "- ✘ - Missing necesary approvals from group [" <> show groupName <>
                                    "](link_to_approvers.yaml). Specifically:" <> show users <> ".\n"


posFb :: PosFeedback -> String
posFb (PosFeedback fb) =
  -- ideally we would have links to the approve comments also
  "- ✔ - Approvals for group [" <> show fb.groupName <>
  "](link_to_approvers.yaml) satisfied based on approves from: " <> show fb.users <> ".\n"


-- mergeMessage [(PosFeedback {groupName: "something", condition : AtLeast 1, users : ["James", "Martin"]})]

