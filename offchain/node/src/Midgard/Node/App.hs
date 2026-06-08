module Midgard.Node.App (
  runMigrate,
  runServe,
  runVerify,
) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import Data.Text qualified as Text
import Database.Persist.Postgresql (SqlBackend)
import Midgard.Node.Config qualified as Config
import Midgard.Node.DB.Migration qualified as DB.Migration
import Midgard.Node.DB.Pool qualified as DB.Pool
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Midgard.Node.Server qualified as Server
import Network.Wai.Handler.Warp qualified as Warp

loadRuntime :: FilePath -> IO (Config.MidgardNodeConfig, FilePath, Maybe (Pool SqlBackend))
loadRuntime configPath = do
  config <- Config.loadConfigFile configPath
  migrationDirectory <- Migrations.findSqlMigrationsDirectory
  dbPool <-
    case config.database of
      Nothing -> pure Nothing
      Just dbConfig ->
        Just
          <$> runNoLoggingT
            ( DB.Pool.createPool
                DB.Pool.PostgresConfig
                  { connectionString = dbConfig.connectionString
                  , poolSize = maybe 5 id dbConfig.poolSize
                  }
            )
  pure (config, migrationDirectory, dbPool)

runServe :: FilePath -> IO ()
runServe configPath = do
  (config, migrationDirectory, dbPool) <- loadRuntime configPath
  putStrLn ("midgard-node: using config " <> configPath)
  putStrLn ("midgard-node: using SQL migrations from " <> migrationDirectory)
  case dbPool of
    Nothing -> pure ()
    Just pool -> do
      _ <- DB.Migration.verifyDatabase pool
      pure ()
  let env =
        NodeEnv
          { config
          , migrationDirectory
          , dbPool
          }
  Warp.run config.api.port (Server.mkApplication env)

runMigrate :: FilePath -> IO ()
runMigrate configPath = do
  (config, _, maybePool) <- loadRuntime configPath
  case maybePool of
    Nothing -> fail "midgard-node migrate requires a database config"
    Just pool -> do
      DB.Migration.migrateDatabase pool
      putStrLn ("midgard-node: migrations applied for network " <> Text.unpack config.midgard.network)

runVerify :: FilePath -> IO ()
runVerify configPath = do
  (_, _, maybePool) <- loadRuntime configPath
  case maybePool of
    Nothing -> fail "midgard-node verify requires a database config"
    Just pool -> print =<< DB.Migration.verifyDatabase pool
