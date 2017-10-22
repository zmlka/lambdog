module IsApproval where

import Prelude

import Data.Array (filter, length)

isApproval :: String -> Boolean
isApproval "/approve" = true
isApproval _ = false

isApprovalCount :: Array String -> Int
isApprovalCount xs =
  xs
  # filter isApproval
  # length
