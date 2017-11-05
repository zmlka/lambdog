module ShouldMerge where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, error)
import Control.Monad.Except (catchError, runExcept, throwError)
import Data.Array (catMaybes, nub, filter, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl)
import Data.Foreign (F, Foreign, readArray, readInt, readString)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, lookup)
import Data.Yaml (load)
import GitHub.Api (Comment(..), User, getConfigFile)
import Util (err)
import Data.Validation.Semigroup

-- | A condition that need to be met regarding a group of users.
-- | `AtLeast n` : needs `n` approvals from this group.
-- | `All` : needs approvals from everyone in this group.
data Condition
  = AtLeast Int
  | All
derive instance genericCondition :: Generic Condition _
instance showCondition :: Show Condition where show = genericShow

newtype GroupConfig = GroupConfig { groupName :: String
                                  , users :: Array User
                                  , condition :: Condition
                                  }
derive instance genericGroupConfig :: Generic GroupConfig _
instance showGroupConfig :: Show GroupConfig where show = genericShow

-- | There are two sorts feedback:
-- | - negative: rule didn't pass and here is why.
-- | - positive: rule did pass and here is why.

-- | Represents a piece of negative feedback, e.g.
-- | - "Still need 2 approvals from dev"
-- | - "Still need approvals from @andy, @sophie and @greg in dev"
data NegFeedback
  = NeedNumber { groupName :: String, number :: Int }
  | NeedUsers { groupName :: String, users :: Array User }
derive instance genericNegFeedback :: Generic NegFeedback _
instance showNegFeedback :: Show NegFeedback where show = genericShow

-- -- | Represents an entire negative feedback for a whole rule.
-- newtype NegRuleFeedback = NegRuleFeedback (Array NegFeedback)
-- derive instance genericNegRuleFeedback :: Generic NegRuleFeedback _
-- instance showNegRuleFeedback :: Show NegRuleFeedback where show = genericShow

-- | A positive feedback is a condition and the set of users which approved
-- | which made that condition hold.
newtype PosFeedback = PosFeedback { groupName :: String
                                  , condition :: Condition
                                  , users :: Array User
                                  }
derive instance genericPosFeedback :: Generic PosFeedback _
instance showPosFeedback :: Show PosFeedback where show = genericShow

-- newtype PosRuleFeedback = PosRuleFeedback (Array PosFeedback)
-- derive instance genericPosRuleFeedback :: Generic PosRuleFeedback _
-- instance showPosRuleFeedback :: Show PosRuleFeedback where show = genericShow

-- | A data representation of the `config.yaml` file.
newtype Config = Config (Array GroupConfig)
derive instance genericConfig :: Generic Config _
instance showConfig :: Show Config where show = genericShow

-- | The main function:
-- | Given a list of PR comments and a config, decides if the PR should be
-- | merged. Returns feedback relevant to decision.
shouldMerge :: Array Comment -> Config -> Either (Array NegFeedback) (Array PosFeedback)
shouldMerge comments (Config config) =
    unV Left (Right <<< join) (traverse (groupOk comments) config)

groupOk :: Array Comment -> GroupConfig -> V (Array NegFeedback) (Array PosFeedback)
groupOk comments (GroupConfig config) =
    case config.condition of
      AtLeast n ->
        if length approves >= n
           then pure [ PosFeedback
                         { groupName: config.groupName
                         , condition: config.condition
                         , users: approves } ]
           else invalid [ NeedNumber
                            { groupName: config.groupName
                            , number: n - length approves } ]
      All -> let needed = filter (_ `elem` approves) config.users
             in if needed == []
                   then pure [ PosFeedback
                                 { groupName: config.groupName
                                 , condition: config.condition
                                 , users: config.users } ]
                   else invalid [ NeedUsers
                                    { groupName: config.groupName
                                    , users: needed } ]
  where
    -- Usernames of relevant approvals (deduped)
    approves = comments
      # filter (\(Comment c) -> c.commentText == "/approve" && elem c.user config.users)
      # map (\(Comment c) -> c.user)
      # nub

--
-- Code to get the YAML files for a repo and parse them into the corresponding
-- datatypes follows.
--

newtype Criterion = Criterion { groupName :: String, condition :: Condition }
derive instance newtypeCriterion :: Newtype Criterion _
derive instance genericCriterion :: Generic Criterion _
instance showCriterion :: Show Criterion where show = genericShow

readAll :: Foreign -> F Condition
readAll f = do
  s <- readString f
  if s == "all" then pure All else err "String as condition that wasn't 'all'."

condPair :: Foreign -> String -> F Criterion
condPair f u = do
    c <- (f ! u >>= readCondition) `catchError` \e -> err ("condPair, error at key: " <> u <> " : " <> show e)
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

# Dveloppers
dev:
- jaameshaydon
- zmlka
"""

testApprovers :: Either String (Array ApprovalGroup)
testApprovers = yamlApprovalGroups testApproversYaml

testConfig :: Either String Config
testConfig = yamlConfig testCriterionYaml testApproversYaml

{-
groupOk [{user: "james", commentText: "/approve"}] (GroupConfig { groupName: "dev", users: ["james"], condition: AtLeast 1})

groupOk [ {user: "martin", commentText: "lol"}
        , {user: "james", commentText: "what?"}
        , {user: "martin", commentText: "/approve"}
        ]
        (GroupConfig { groupName: "dev"
        , users: ["james", "martin"]
        , condition: AtLeast 1})

groupOk [ {user: "martin", commentText: "lol"}
        , {user: "james", commentText: "what?"}
        , {user: "martin", commentText: "/approve"}
        ]
        (GroupConfig { groupName: "dev"
        , users: ["james", "martin"]
        , condition: All})

shouldMerge [{user: "james", commentText: "/approve"}] (config [(GroupConfig { groupName: "dev", users: ["james"], condition: AtLeast 1})])


shouldMerge [{user: "james", commentText: "/approve"}] (config [(GroupConfig { groupName: "dev", users: ["james"], condition: AtLeast 1})], [(GroupConfig { groupName: "ops", users: ["james", "martin"], condition: All})])

-}
