module ShouldMerge where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, nub)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
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

type Config = Array { groupName :: String, users :: Array User, condition :: Condition }

type Comment =
  {
    user :: String,
    commentText :: String
  }


{-
shouldMerge1 :: Config -> Array Comment -> Boolean
shouldMerge1 config comments =
  let aa =
        comments
        # foldl (shouldMergeHelper1) []
        # length
  in
   aa >= length config
-}

{-
shouldMergeHelper1 :: Config -> Comment -> Comment
shouldMergeHelper1 config comment =
  case config of
    [] -> ?lol -- some error exit
    (x:xs) -> if comment.user == x then x
               else shouldMergeHelper xs comment
-}

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
    c <- f ! u >>= readCondition
    pure $ Criterion { groupName: u, condition: c }
  where
    readCondition f = (AtLeast <$> readInt f) <|> readAll f

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

combi :: ApprovalGroup -> Criterion -> Maybe { groupName :: String, users :: Array User, condition :: Condition }
combi (ApprovalGroup a) (Criterion c) =
  if a.groupName == c.groupName
     then Just { groupName: a.groupName
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
  in catMaybes $ go <$> ks

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
  approvers <- getConfigFile "approvers.yaml"
                             repo
  let config = yamlConfig crit approvers
  case config of
    Left e -> throwError (error e)
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

# Developpers
dev:
- jameshaydon
- zmlka
"""

testApprovers :: Either String (Array ApprovalGroup)
testApprovers = yamlApprovalGroups testApproversYaml

testConfig :: Either String Config
testConfig = yamlConfig testCriterionYaml testApproversYaml