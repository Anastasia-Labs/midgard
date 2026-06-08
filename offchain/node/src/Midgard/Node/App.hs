module Midgard.Node.App (run) where

import Midgard.Node.Config qualified as Config
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Midgard.Node.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp

run :: FilePath -> IO ()
run configPath = do
  config <- Config.loadConfigFile configPath
  migrationDirectory <- Migrations.findSqlMigrationsDirectory
  putStrLn ("midgard-node: using config " <> configPath)
  putStrLn ("midgard-node: using SQL migrations from " <> migrationDirectory)
  let env =
        NodeEnv
          { config
          , migrationDirectory
          }
  Warp.run config.api.port (Server.mkApplication env)
