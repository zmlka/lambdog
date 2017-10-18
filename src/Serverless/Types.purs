module Serverless.Types where

import Prelude
import Data.Foreign (ForeignError(..), readString, fail)
import Data.Foreign.Class (class Decode)
import Data.String.Regex (Regex)
import Control.Monad.Eff (Eff, kind Effect)

foreign import data EXPRESS :: Effect

-- | General monad, indicates that we're dealing with
-- | express.js related functions.
-- | Applications should use HandlerM and AppM primarily
-- | and ExpressM in rare cases.
type ExpressM e a = Eff (express :: EXPRESS | e) a


foreign import data Application :: Type
foreign import data Event :: Type
foreign import data Response :: Type
foreign import data Request :: Type

data Protocol = Http | Https

instance showProtocol :: Show Protocol where
    show Http  = "http"
    show Https = "https"

instance isForeignProtocol :: Decode Protocol where
    decode value = readString value >>= case _ of
        "http"  -> pure Http
        "https" -> pure Https
        _ -> fail $ JSONError "Unknown protocol"


data Method = ALL | GET | POST | PUT | DELETE | OPTIONS | HEAD | TRACE | CustomMethod String

instance showMethod :: Show Method where
    show ALL     = "all"
    show GET     = "get"
    show POST    = "post"
    show PUT     = "put"
    show DELETE  = "delete"
    show OPTIONS = "options"
    show HEAD    = "head"
    show TRACE   = "trace"
    show (CustomMethod method) = method

instance isForeignMethod :: Decode Method where
    decode value = readString value >>= case _ of
        "GET"     -> pure GET
        "POST"    -> pure POST
        "PUT"     -> pure PUT
        "DELETE"  -> pure DELETE
        "OPTIONS" -> pure OPTIONS
        "HEAD"    -> pure HEAD
        "TRACE"   -> pure TRACE
        method    -> pure $ CustomMethod method

type Port = Int
type Pipe = String
type Path = String

class RoutePattern a
instance routePath  :: RoutePattern String
instance routeRegex :: RoutePattern Regex

class RequestParam a
instance requestParamString :: RequestParam String
instance requestParamNumber :: RequestParam Number

-- | Cookie options
-- | - maxAge -- time in msecs
-- | - signed -- use secret to sign if true
-- | - path   -- cookie path
newtype CookieOptions = CookieOptions
    { maxAge :: Int
    , signed :: Boolean
    , path :: String
    }
