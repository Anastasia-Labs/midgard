module Midgard.Node.DB.Utils (runWithConnStr) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (NoLoggingT (NoLoggingT, runNoLoggingT))
import Control.Monad.Reader (mapReaderT)

import Database.Persist.Postgresql (
  ConnectionString,
  runSqlConn,
  withPostgresqlConn,
 )
import Database.Persist.Sql (SqlPersistT)

{- | Simple and quick helper to run sql code with minimal setup.
Use production ready persistent helpers for actual code. This is for testing.
-}
runWithConnStr :: (MonadUnliftIO m) => ConnectionString -> SqlPersistT m a -> m a
runWithConnStr connStr action =
  runNoLoggingT . withPostgresqlConn connStr $
    runSqlConn (mapReaderT NoLoggingT action)
