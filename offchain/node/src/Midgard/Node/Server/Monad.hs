module Midgard.Node.Server.Monad (ServerM (..), runServerM) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader

import Servant (Handler, ServerError)

import Midgard.Node.Env

newtype ServerM a = ServerM
  { unServerM :: ReaderT NodeEnv Handler a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader NodeEnv
    , MonadError ServerError
    , MonadIO
    )

runServerM :: NodeEnv -> ServerM a -> Handler a
runServerM env app = runReaderT app.unServerM env
