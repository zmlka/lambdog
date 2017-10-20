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


type Config =
  {
    deciders :: Array User 
  }

type Comment =
  {
    user :: String,
    commentText :: String
  }


shouldMerge1 :: Config -> Array Comment -> Boolean
shouldMerge1 config comments =
  let aa =
        comments
        # array.foldl (shouldMergeHelper) []
        # array.length
  in
   aa >= array.length config


shouldMergeHelper1 :: Config -> Comment -> Comment
shouldMergeHelper1 config comment =
  case config of
    [] -> -- some error exit
    (x::xs) -> if comment.user == x then x
               else shouldMergeHelper xs comment


--- past this line it is more exocic
--- past this line it is more exocic
--- past this line it is more exocic

-- config is actually a list of lists + quantities
