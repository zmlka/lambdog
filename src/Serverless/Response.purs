module Serverless.Response where

import Prelude
import Data.Function.Uncurried (Fn3, Fn4, Fn2)
import Data.Foreign (Foreign)
import Control.Monad.Eff.Exception (Error)
import Serverless.Types (ExpressM, Response, CookieOptions)

foreign import _cwd :: Unit -> String

foreign import _setStatus :: forall e. Fn2 Response Int (ExpressM e Unit)

foreign import _setContentType :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _getHeader :: forall e. Fn2 Response String (ExpressM e Foreign)

foreign import _setHeader :: forall e a. Fn3 Response String a (ExpressM e Unit)

foreign import _setCookie :: forall e. Fn4 Response String String CookieOptions (ExpressM e Unit)

foreign import _clearCookie :: forall e. Fn3 Response String String (ExpressM e Unit)

foreign import _end :: forall e. Response -> (ExpressM e Unit)

foreign import _send :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _sendJson :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _sendJsonp :: forall e a. Fn2 Response a (ExpressM e Unit)

foreign import _redirectWithStatus :: forall e. Fn3 Response Int String (ExpressM e Unit)

foreign import _setLocation :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _setAttachment :: forall e. Fn2 Response String (ExpressM e Unit)

foreign import _sendFileExt :: forall e opts. Fn4 Response String { | opts } (Error -> ExpressM e Unit) (ExpressM e Unit)

foreign import _downloadExt :: forall e. Fn4 Response String String (Error -> ExpressM e Unit) (ExpressM e Unit)

foreign import _headersSent :: forall e. Response -> ExpressM e Boolean

foreign import _render :: forall e a. Fn3 Response String a (ExpressM e Unit)
