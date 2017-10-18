module Serverless.Request where

import Prelude
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2(), Fn3())
import Control.Monad.Eff (Eff, kind Effect)

import Serverless.Types

foreign import _getRouteParam :: forall e a. Fn2 Request a (ExpressM e Foreign)

foreign import _getRoute :: forall e. Request -> ExpressM e String

foreign import _getBody :: forall e. Request -> ExpressM e Foreign

foreign import _getBodyParam :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getQueryParams :: forall e. Request -> ExpressM e String

foreign import _getCookie :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getSignedCookie :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getHeader :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _accepts :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _acceptsCharset :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _acceptsLanguage :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _hasType :: forall e. Fn2 Request String (ExpressM e Foreign)

foreign import _getRemoteIp :: forall e. Request -> ExpressM e String

foreign import _getRemoteIps :: forall e. Request -> ExpressM e (Array String)

foreign import _getPath :: forall e. Request -> ExpressM e String

foreign import _getHostname :: forall e. Request -> ExpressM e String

foreign import _getSubdomains :: forall e. Request -> ExpressM e (Array String)

foreign import _isFresh :: forall e. Request -> ExpressM e Boolean

foreign import _isStale :: forall e. Request -> ExpressM e Boolean

foreign import _isXhr :: forall e. Request -> ExpressM e Boolean

foreign import _getProtocol :: forall e. Request -> ExpressM e Foreign

foreign import _getMethod :: forall e. Request -> ExpressM e Foreign

foreign import _getUrl :: forall e. Request -> ExpressM e String

foreign import _getOriginalUrl :: forall e. Request -> ExpressM e String

foreign import _setData :: forall a e. Fn3 Request String a (ExpressM e Unit)

foreign import _getData :: forall e. Fn2 Request String (ExpressM e Foreign)
