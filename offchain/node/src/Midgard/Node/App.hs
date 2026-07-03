module Midgard.Node.App (
  runServe,
) where

import Control.Monad.Logger (NoLoggingT (NoLoggingT), runNoLoggingT)
import Database.Persist.Postgresql (withPostgresqlPool)
import Network.Wai.Handler.Warp qualified as Warp

import Midgard.Node.Config (loadConfigFile, unDbConnStr)
import Midgard.Node.Config qualified as Config
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Server qualified as Server

runServe :: FilePath -> IO ()
runServe configPath = do
  config <- loadConfigFile configPath
  let dbConf = config.database
  runNoLoggingT . withPostgresqlPool (unDbConnStr dbConf.connectionString) dbConf.poolSize $
    \dbPool -> NoLoggingT $ do
      putStrLn $ "midgard-node: using config " <> configPath
      let env =
            NodeEnv
              { config
              , dbPool
              }
      Warp.run config.api.port (Server.mkApplication env)
