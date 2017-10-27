module HomeoWork where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (catchError, runExcept, throwError)
import Data.Array (catMaybes, nub)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readInt, readString)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, lookup)
import Data.Yaml (load)
import GitHub.Api (getConfigFile)

type User = String

newtype GroupConfig = GroupConfig { groupName :: String, users :: Array User, condition :: Condition }

newtype Config = Config (Array GroupConfig)

type Comment =
  { user :: String
  , commentText :: String
  }

-- | Mocked for now, just searches for a single approve comment.
shouldMerge :: Array Comment -> Config -> Boolean
shouldMerge comments _ = any (\c -> c.commentText == "/approve") comments

groupOk :: Array Comment -> GroupConfig -> Boolean
groupOk comments (GroupConfig config) =
  let
    removeIrrelevantComments :: Array User -> Array Comments -> ArrayComment
    removeIrrelevantComments cfg comments =
      comments
      # filter (\c -> c.commentText == "/approve")
      -- compare list of users in comments (c.user) to list of users in config (config.users)
      # filter (elem c.user config.users)
      # nub
  in
   if config.condition == Int then
      comments
      # removeIrrelevantComments config.users
      # length >= config.condition
   if config.conditoin == "all" then
      comments
      # removeIrrelevantComments config.users
      # length == length config.users
   else false

-- here are some tests:
-- 
-- groupOk (GroupConfig { groupName: "dev", users: ["james"], condition: AtLeast 1})
--         [{user: "james", commentText: "/approve"}]
-- 
-- (edited)
-- groupOk (GroupConfig { groupName: "dev"
--                      , users: ["james", "martin"]
--                      , condition: AtLeast 1})
--         [ {user: "martin", commentText: "lol"}
--         , {user: "james", commentText: "what?"}
--         , {user: "martin", commentText: "/approve"}
--         ]
-- 
-- should also return true
-- 
-- groupOk (GroupConfig { groupName: "dev"
--                      , users: ["james", "martin"]
--                      , condition: All})
--         [ {user: "martin", commentText: "lol"}
--         , {user: "james", commentText: "what?"}
--         , {user: "martin", commentText: "/approve"}
--         ]
-- 
-- should return false

