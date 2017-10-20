module IsApproval where

import GitHub.Api
import Prelude

import Control.Monad.Aff (Aff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (toForeign)
import Debug.Trace (traceAny, traceShow)
import Data.Array

type User = String

-- | A condition that need to be met regarding a group of users.
-- | `AtLeast n` : needs `n` approvals from this group.
-- | `All` : needs approvals from everyone in this group.
data Condition
  = AtLeast Int
  | All

type Config = Array { groupName :: String, groupUsers :: Array User, condition :: Condition }

type Comment =
  {
    user :: String,
    commentText :: String
  }


shouldMerge1 :: Config -> Array Comment -> Boolean
shouldMerge1 config comments =
  let aa =
        comments
        # foldl (shouldMergeHelper1) []
        # length
  in
   aa >= length config


shouldMergeHelper1 :: Config -> Comment -> Comment
shouldMergeHelper1 config comment =
  case config of
    [] -> ?lol -- some error exit
    (x:xs) -> if comment.user == x then x
               else shouldMergeHelper xs comment


--- past this line it is more exocic
--- past this line it is more exocic
--- past this line it is more exocic

-- config is actually a list of lists + quantities
