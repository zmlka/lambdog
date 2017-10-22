module ShouldMerge where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (throwError)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foreign (F, Foreign, ForeignError(..), readInt, readString)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NEL
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Yaml (load)

type User = String

-- | A condition that need to be met regarding a group of users.
-- | `AtLeast n` : needs `n` approvals from this group.
-- | `All` : needs approvals from everyone in this group.
data Condition
  = AtLeast Int
  | All

derive instance genericCondition :: Generic Condition _
instance showCondition :: Show Condition where show = genericShow

type Config = Array { groupName :: String, groupUsers :: Array User, condition :: Condition }

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

type UserGroup = String

newtype Criterion = Criterion { userGroup :: UserGroup, condition :: Condition }

derive instance newtypeCriterion :: Newtype Criterion _
derive instance genericCriterion :: Generic Criterion _

instance showCriterion :: Show Criterion where show = genericShow

readAll :: Foreign -> F Condition
readAll f = do
  s <- readString f
  if s == "all" then pure All else throwError (NEL.singleton (ForeignError "String as condition that wasn't 'all'."))

condPair :: Foreign -> UserGroup -> F Criterion
condPair f u = do
    c <- f ! u >>= readCondition
    pure $ Criterion { userGroup: u, condition: c }
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

-- Tests

testYaml :: String
testYaml = """
---

# define which approval groups need how many approvers
approvergroups:
  ops: all
  dev: 2"""

test :: Either String (Array Criterion)
test = yamlCriterions testYaml
