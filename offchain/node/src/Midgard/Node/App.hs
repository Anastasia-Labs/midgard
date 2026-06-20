module Midgard.Node.App (
  runMigrate,
  runServe,
  runVerify,
) where

import Midgard.Node.Config qualified as Config
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Midgard.Node.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp

-- Load all runtime pieces shared by `serve`, `migrate`, and `verify`.
-- The SQL migration directory lives in the TypeScript node for now, so we
-- resolve that here once and thread it through the rest of the process.
loadRuntime :: FilePath -> IO (Config.MidgardNodeConfig, FilePath)
loadRuntime configPath = do
  config <- Config.loadConfigFile configPath
  migrationDirectory <- Migrations.findSqlMigrationsDirectory
  pure (config, migrationDirectory)

runServe :: FilePath -> IO ()
runServe configPath = do
  (config, migrationDirectory) <- loadRuntime configPath
  putStrLn ("midgard-node: using config " <> configPath)
  putStrLn ("midgard-node: using SQL migrations from " <> migrationDirectory)
  let env =
        NodeEnv
          { config
          , migrationDirectory
          }
  Warp.run config.api.port (Server.mkApplication env)

runMigrate :: FilePath -> IO ()
runMigrate _configPath =
  -- TODO(db): re-enable once the Haskell node DB layer is brought back into the
  -- cabal graph. SQL files still live under ../demo/midgard-node for now.
  fail "midgard-node migrate is temporarily disabled while DB integration is parked"

runVerify :: FilePath -> IO ()
runVerify _configPath =
  -- TODO(db): restore schema verification together with migrations.
  fail "midgard-node verify is temporarily disabled while DB integration is parked"
