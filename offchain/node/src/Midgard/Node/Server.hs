module Midgard.Node.Server (mkApplication) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Midgard.Node.API (
  AdminAPI,
  HealthAPI,
  HealthResponse (..),
  MidgardNodeAPI,
  ProtocolInfoResponse (..),
  QueryAPI,
  ReadyResponse (..),
  TxAPI,
  UtxoLookupRequest (..),
 )
import Midgard.Node.Config qualified as Config
import Midgard.Node.DB.Health qualified as DB.Health
import Midgard.Node.DB.Migration qualified as DB.Migration
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Network.Wai (Application)
import Servant (
  Handler,
  ServerT,
  hoistServer,
  serve,
  type (:<|>) (..),
 )

newtype AppM a = AppM
  { unAppM :: ReaderT NodeEnv Handler a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runAppM :: NodeEnv -> AppM a -> Handler a
runAppM env app = runReaderT app.unAppM env

mkApplication :: NodeEnv -> Application
mkApplication env =
  serve api (hoistServer api (runAppM env) server)

api :: Proxy MidgardNodeAPI
api = Proxy

server :: ServerT MidgardNodeAPI AppM
server =
  healthServer
    :<|> queryServer
    :<|> adminServer
    :<|> txServer

healthServer :: ServerT HealthAPI AppM
healthServer =
  healthHandler
    :<|> readinessHandler

queryServer :: ServerT QueryAPI AppM
queryServer =
  protocolInfoHandler
    :<|> placeholderQueryHandler "tx"
    :<|> placeholderQueryHandler "tx-status"
    :<|> placeholderDepositStatusHandler
    :<|> placeholderQueryHandler "txs"
    :<|> placeholderQueryHandler "utxo"
    :<|> placeholderQueryHandler "utxos"
    :<|> placeholderUtxosPostHandler
    :<|> placeholderQueryHandler "block"

adminServer :: ServerT AdminAPI AppM
adminServer =
  notImplementedHandler "init"
    :<|> notImplementedHandler "commit"
    :<|> notImplementedHandler "merge"
    :<|> notImplementedHandler "stateQueue"
    :<|> notImplementedHandler "logBlocksDB"
    :<|> notImplementedHandler "logGlobals"

txServer :: ServerT TxAPI AppM
txServer =
  placeholderJsonBodyHandler "deposit/build"
    :<|> placeholderJsonBodyHandler "submit"

healthHandler :: AppM HealthResponse
healthHandler = pure (HealthResponse "ok")

readinessHandler :: AppM ReadyResponse
readinessHandler = do
  env <- AppM (asks id)
  migrationFiles <- liftIO Migrations.listSqlMigrations
  -- For now readiness means:
  --   1. we can still find the shared SQL migration files,
  --   2. the DB responds to a trivial query when configured, and
  --   3. schema verification succeeds when configured.
  --
  -- Later this endpoint should also include worker heartbeats and queue depth
  -- checks like the TypeScript node.
  dbHealthy <- liftIO $
    case env.dbPool of
      Nothing -> pure True
      Just pool -> DB.Health.ping pool
  schemaReady <- liftIO $
    case env.dbPool of
      Nothing -> pure (not (null migrationFiles))
      Just pool -> do
        result <- try (DB.Migration.verifyDatabase pool)
        pure $
          case result of
            Left (_ :: SomeException) -> False
            Right status -> status.compatible
  let reasons =
        ["sql_migrations_missing" | null migrationFiles]
          <> ["db_unhealthy" | not dbHealthy]
          <> ["schema_incompatible" | not schemaReady]
  pure
    ReadyResponse
      { ready = dbHealthy && schemaReady
      , reasons
      , dbConfigured = maybe False (const True) env.config.database
      , schemaSource = env.migrationDirectory
      }

protocolInfoHandler :: AppM ProtocolInfoResponse
protocolInfoHandler = do
  env <- AppM (asks id)
  pure
    ProtocolInfoResponse
      { network = env.config.midgard.network
      , mutatingEndpointsEnabled = env.config.features.enableMutatingEndpoints
      , migrationDirectory = env.migrationDirectory
      }

placeholderQueryHandler :: Text -> Maybe Text -> AppM Aeson.Value
placeholderQueryHandler endpoint maybeArgument =
  -- Keep the route surface available while we port handlers incrementally.
  pure $
    Aeson.object
      [ "endpoint" Aeson..= endpoint
      , "status" Aeson..= ("not_implemented" :: Text)
      , "argument" Aeson..= maybeArgument
      ]

placeholderDepositStatusHandler :: Maybe Text -> Maybe Text -> AppM Aeson.Value
placeholderDepositStatusHandler eventId l1TxHash =
  pure $
    Aeson.object
      [ "endpoint" Aeson..= ("deposit-status" :: Text)
      , "status" Aeson..= ("not_implemented" :: Text)
      , "eventId" Aeson..= eventId
      , "l1TxHash" Aeson..= l1TxHash
      ]

placeholderUtxosPostHandler :: UtxoLookupRequest -> AppM Aeson.Value
placeholderUtxosPostHandler request =
  pure $
    Aeson.object
      [ "endpoint" Aeson..= ("utxos" :: Text)
      , "status" Aeson..= ("not_implemented" :: Text)
      , "txOutRefs" Aeson..= request.txOutRefs
      ]

placeholderJsonBodyHandler :: Text -> Aeson.Value -> AppM Aeson.Value
placeholderJsonBodyHandler endpoint body =
  pure $
    Aeson.object
      [ "endpoint" Aeson..= endpoint
      , "status" Aeson..= ("not_implemented" :: Text)
      , "request" Aeson..= body
      ]

notImplementedHandler :: Text -> AppM Aeson.Value
notImplementedHandler endpoint =
  pure $
    Aeson.object
      [ "endpoint" Aeson..= endpoint
      , "status" Aeson..= ("not_implemented" :: Text)
      ]
