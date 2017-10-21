module Util where

import Prelude

foreign import decodeBase64 :: String -> String

foo :: Int -> Int -> Int
foo x y = 2 * x + y
