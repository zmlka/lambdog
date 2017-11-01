module ShouldMerge where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (catchError, runExcept, throwError)
import Data.Array (catMaybes, nub, filter, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (any, elem)
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

-- | A condition that need to be met regarding a group of users.
-- | `AtLeast n` : needs `n` approvals from this group.
-- | `All` : needs approvals from everyone in this group.
data Condition
  = AtLeast Int
  | All

derive instance genericCondition :: Generic Condition _
instance showCondition :: Show Condition where show = genericShow

newtype GroupConfig = GroupConfig { groupName :: String, users :: Array User, condition :: Condition }

derive instance genericGroupConfig :: Generic GroupConfig _
instance showGroupConfig :: Show GroupConfig where show = genericShow

newtype Config = Config (Array GroupConfig)

derive instance genericConfig :: Generic Config _
instance showConfig :: Show Config where show = genericShow

type Comment =
  { user :: String
  , commentText :: String
  }

type UserGroupId = String

newtype Criterion = Criterion { groupName :: UserGroupId, condition :: Condition }

derive instance newtypeCriterion :: Newtype Criterion _
derive instance genericCriterion :: Generic Criterion _
instance showCriterion :: Show Criterion where show = genericShow

readAll :: Foreign -> F Condition
readAll f = do
  s <- readString f
  if s == "all" then pure All else throwError (NEL.singleton (ForeignError "String as condition that wasn't 'all'."))

condPair :: Foreign -> UserGroupId -> F Criterion
condPair f u = do
    c <- (f ! u >>= readCondition) `catchError` \e -> throwError (NEL.singleton (ForeignError ("condPair, error at key: " <> u <> " : " <> show e)))
    pure $ Criterion { groupName: u, condition: c }
  where
    readCondition f_ = (AtLeast <$> readInt f_) <|> readAll f_

readCriterions :: Foreign -> F (Array Criterion)
readCriterions f = do
  groups <- f ! "approvergroups"
  ks <- keys groups
  traverse (condPair groups) ks

-- | Parses YAML specifying approval conditions into an array of Criterion.
yamlCriterions :: String -> Either String (Array Criterion)
yamlCriterions s = do
  f <- load s
  lmap show (runExcept (readCriterions f))

newtype ApprovalGroup = ApprovalGroup { groupName :: String
                                      , users :: Array User
                                      }
derive instance newtypeApprovalGroup :: Newtype ApprovalGroup _
derive instance genericApprovalGroup :: Generic ApprovalGroup _
instance showApprovalGroup :: Show ApprovalGroup where show = genericShow

readApprovalGroup :: Foreign -> String -> F ApprovalGroup
readApprovalGroup f name = do
  xs <- f ! name >>= readArray
  us <- traverse readString xs
  pure (ApprovalGroup { groupName: name, users: us })

readApprovalGroups :: Foreign -> F (Array ApprovalGroup)
readApprovalGroups f = do
  ks <- keys f
  traverse (readApprovalGroup f) ks

yamlApprovalGroups :: String -> Either String (Array ApprovalGroup)
yamlApprovalGroups s = do
  f <- load s
  lmap show (runExcept (readApprovalGroups f))

gn :: forall a l. Newtype a { groupName :: String | l } => a -> Tuple String a
gn x = Tuple ((unwrap x).groupName) x

combi :: ApprovalGroup -> Criterion -> Maybe GroupConfig
combi (ApprovalGroup a) (Criterion c) =
  if a.groupName == c.groupName
     then Just $ GroupConfig { groupName: a.groupName
                             , users: a.users
                             , condition: c.condition }
     else Nothing

makeConfig :: Array ApprovalGroup -> Array Criterion -> Config
makeConfig as cs =
  let aM = gn <$> as
      cM = gn <$> cs
      ks = nub ((fst <$> aM) <> (fst <$> cM))
      go k = do a <- lookup k aM
                c <- lookup k cM
                combi a c
  in Config $ catMaybes $ go <$> ks

-- | Combines two YAML files to produce a config.
yamlConfig :: String -> String -> Either String Config
yamlConfig cY aY = do
  a <- yamlApprovalGroups aY
  c <- yamlCriterions cY
  pure (makeConfig a c)

getRepoConfig :: forall e.
     { targetRepo :: String
     , configRepo :: String
     , owner :: String
     , targetBranch :: String
     , configBranch :: String }
  -> Aff e Config
getRepoConfig repo = do
  crit <- getConfigFile "config.yaml"
                        repo
          `catchError` \err -> throwError (error ("Error in getting crit: " <> show err))
  approvers <- getConfigFile "approvers.yaml"
                             repo
               `catchError` \err -> throwError (error ("Error in getting approvers: " <> show err))
  let config = yamlConfig crit approvers
  case config of
    Left e -> throwError (error ("Error in getRepoConfig: " <> e))
    Right r -> pure r

-- | Mocked for now, just searches for a single approve comment.
shouldMerge :: Array Comment -> Config -> Boolean
shouldMerge comments _ = any (\c -> c.commentText == "/approve") comments


groupOk :: Array Comment -> GroupConfig -> Boolean
groupOk comments (GroupConfig config) =
  let
    removeIrrelevantComments :: Array User -> Array Comment -> Array String
    removeIrrelevantComments cfg comments =
      comments
      # filter (\c -> c.commentText == "/approve")
      # filter (\c -> elem c.user config.users)
      # map (\c -> c.user)
      # nub
  in
   case config.condition of
     AtLeast n ->
       comments
       # removeIrrelevantComments config.users
       # \cs -> length cs >= n
     All ->
       comments
       # removeIrrelevantComments config.users
       # \cs -> length cs >= length config.users

-- Tests

testCriterionYaml :: String
testCriterionYaml = """---

# define which approval groups need how many approvers
approvergroups:
  ops: all
  dev: 2"""

testCriterion :: Either String (Array Criterion)
testCriterion = yamlCriterions testCriterionYaml

testApproversYaml :: String
testApproversYaml = """---

# Operations team
ops:
- zmlka

# Developpers
dev:
- jameshaydon
- zmlka
"""

testApprovers :: Either String (Array ApprovalGroup)
testApprovers = yamlApprovalGroups testApproversYaml

testConfig :: Either String Config
testConfig = yamlConfig testCriterionYaml testApproversYaml

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
