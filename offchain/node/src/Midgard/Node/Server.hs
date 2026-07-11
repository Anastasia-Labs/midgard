module Midgard.Node.Server (
  ServerM (..),
  mkApplication,
  runServerM,
) where

import Data.Proxy (Proxy (..))
import Midgard.Node.Env (NodeEnv)
import Midgard.Node.Server.Admin (adminServer)
import Midgard.Node.Server.Api (MidgardNodeAPI)
import Midgard.Node.Server.Health (healthServer)
import Midgard.Node.Server.Monad (ServerM (..), runServerM)
import Midgard.Node.Server.Query (queryServer)
import Midgard.Node.Server.Tx (txServer)
import Network.Wai (Application)
import Servant (ServerT, hoistServer, serve, type (:<|>) (..))

mkApplication :: NodeEnv -> Application
mkApplication env =
  serve midgardNodeAPI (hoistServer midgardNodeAPI (runServerM env) midgardNodeServer)

midgardNodeAPI :: Proxy MidgardNodeAPI
midgardNodeAPI = Proxy

midgardNodeServer :: ServerT MidgardNodeAPI ServerM
midgardNodeServer =
  healthServer
    :<|> queryServer
    :<|> adminServer
    :<|> txServer
