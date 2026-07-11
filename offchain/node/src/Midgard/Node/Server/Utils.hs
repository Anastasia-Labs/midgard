module Midgard.Node.Server.Utils (jsonError) where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Servant.Server (ServerError (errBody))

import Midgard.Node.Server.Monad (ServerM)

-- | Utility to return a '{ error: msg }' json object.
jsonError :: (Aeson.ToJSON msg) => ServerError -> msg -> ServerM a
jsonError err msg = throwError err {errBody = encodedError}
  where
    encodedError = Aeson.encode $ Aeson.object [Key.fromString "error" .= msg]
