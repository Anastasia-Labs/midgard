module Midgard.Node.Server.Admin (
  adminServer,
) where

import Data.Text (Text)
import Midgard.Node.Server.Admin.Types (MessageResponse (..), StateQueueResponse (..))
import Midgard.Node.Server.Api (AdminAPI)
import Midgard.Node.Server.Monad (ServerM)
import Servant (ServerT, type (:<|>) (..))

adminServer :: ServerT AdminAPI ServerM
adminServer =
  notImplementedMessageHandler "init"
    :<|> notImplementedMessageHandler "commit"
    :<|> notImplementedMessageHandler "merge"
    :<|> stateQueuePlaceholderHandler
    :<|> notImplementedMessageHandler "logBlocksDB"
    :<|> notImplementedMessageHandler "logGlobals"

notImplementedMessageHandler :: Text -> ServerM MessageResponse
notImplementedMessageHandler endpoint =
  pure
    MessageResponse
      { message = endpoint <> " is not implemented yet"
      }

stateQueuePlaceholderHandler :: ServerM StateQueueResponse
stateQueuePlaceholderHandler =
  -- TODO(state-queue): vendor/adapt the USDCx NonceListUtxoCache worker and
  -- expose the durable queue headers here.
  pure
    StateQueueResponse
      { headers = []
      }
