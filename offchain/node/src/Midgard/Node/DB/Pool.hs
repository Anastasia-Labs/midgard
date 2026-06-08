module Midgard.Node.DB.Pool (
  PostgresConfig (..),
  createPool,
  runDB,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, replicateM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.Persist.Postgresql (
  PostgresConf (..),
  PostgresConfHooks (..),
  SqlBackend,
  createPostgresqlPoolWithConf,
  defaultPostgresConfHooks,
 )
import Database.Persist.Sql (Single, SqlPersistT, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (Only, query_)

data PostgresConfig = PostgresConfig
  { connectionString :: Text
  , poolSize :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON PostgresConfig where
  parseJSON = withObject "PostgresConfig" $ \obj ->
    PostgresConfig
      <$> obj .: "connectionString"
      <*> obj .:? "poolSize" .!= 5

createPool :: (MonadUnliftIO m, MonadLoggerIO m) => PostgresConfig -> m (Pool SqlBackend)
createPool cfg = do
  pool <- createPostgresqlPoolWithConf pgConf hooks
  liftIO $ void $ forkIO $ forever $ do
    threadDelay (5 * 60 * 1_000_000)
    replicateM_ cfg.poolSize $
      forkIO $
        void $
          try @SomeException $
            runSqlPool (void (rawSql @(Single Int) "SELECT 1" [])) pool
  pure pool
  where
    pgConf = PostgresConf (Text.encodeUtf8 cfg.connectionString) 1 86400 cfg.poolSize
    hooks =
      defaultPostgresConfHooks
        { pgConfHooksAfterCreate = \conn ->
            void (query_ conn "SELECT 1" :: IO [Only Int])
        }

runDB :: (MonadIO m) => Pool SqlBackend -> SqlPersistT IO a -> m a
runDB pool action = liftIO (runSqlPool action pool)
