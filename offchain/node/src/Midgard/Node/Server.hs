module Midgard.Node.Server (mkApplication) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Pool (Pool)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.Persist.Postgresql (SqlBackend)
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
import Midgard.Node.DB.AddressHistory qualified as DB.AddressHistory
import Midgard.Node.DB.Blocks qualified as DB.Blocks
import Midgard.Node.DB.Health qualified as DB.Health
import Midgard.Node.DB.Hex qualified as DB.Hex
import Midgard.Node.DB.MempoolLedger qualified as DB.MempoolLedger
import Midgard.Node.DB.Migration qualified as DB.Migration
import Midgard.Node.DB.Transactions qualified as DB.Transactions
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Network.Wai (Application)
import Servant (
  Handler,
  ServerError (..),
  ServerT,
  err400,
  err404,
  err500,
  hoistServer,
  serve,
  type (:<|>) (..),
 )

newtype AppM a = AppM
  { unAppM :: ReaderT NodeEnv Handler a
  }
  deriving newtype (Applicative, Functor, Monad, MonadError ServerError, MonadIO)

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
    :<|> getTxHandler
    :<|> placeholderQueryHandler "tx-status"
    :<|> placeholderDepositStatusHandler
    :<|> getTxsHandler
    :<|> getUtxoHandler
    :<|> getUtxosHandler
    :<|> placeholderUtxosPostHandler
    :<|> getBlockHandler

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

requireDbPool :: AppM (Pool SqlBackend)
requireDbPool = do
  env <- AppM (asks id)
  case env.dbPool of
    Nothing -> throwError err500 {errBody = "{\"error\":\"database not configured\"}"}
    Just pool -> pure pool

serverErrorWith :: ServerError -> Text -> ServerError
serverErrorWith template message =
  template {errBody = LBS.fromStrict (Text.encodeUtf8 ("{\"error\":\"" <> message <> "\"}"))}

jsonError :: ServerError -> Text -> AppM a
jsonError template message = throwError (serverErrorWith template message)

getTxHandler :: Maybe Text -> AppM Aeson.Value
getTxHandler maybeTxHash = do
  txHashText <- maybe (jsonError err404 "Invalid transaction hash: null") pure maybeTxHash
  txHash <-
    case DB.Hex.decodeTxHashHex txHashText of
      Left _ -> jsonError err404 ("Invalid transaction hash: " <> txHashText)
      Right value -> pure value
  pool <- requireDbPool
  found <- liftIO (DB.Transactions.lookupTxCborByHash pool txHash)
  case found of
    Nothing -> jsonError err404 ("Invalid transaction hash or transaction not found: " <> txHashText)
    Just txCbor ->
      pure $
        Aeson.object
          [ "tx" Aeson..= DB.Hex.encodeHex txCbor
          ]

getBlockHandler :: Maybe Text -> AppM Aeson.Value
getBlockHandler maybeHeaderHash = do
  headerHashText <- maybe (jsonError err400 "Invalid block hash: null") pure maybeHeaderHash
  headerHash <-
    case DB.Hex.decodeHeaderHashHex headerHashText of
      Left _ -> jsonError err400 ("Invalid block hash: " <> headerHashText)
      Right value -> pure value
  pool <- requireDbPool
  hashes <- liftIO (DB.Blocks.lookupBlockTxHashes pool headerHash)
  pure $
    Aeson.object
      [ "hashes" Aeson..= map DB.Hex.encodeHex hashes
      ]

getUtxoHandler :: Maybe Text -> AppM Aeson.Value
getUtxoHandler maybeOutRef = do
  outRefText <- maybe (jsonError err400 "Invalid txOutRef: null") pure maybeOutRef
  outRef <-
    case DB.Hex.decodeTxOutRefHex outRefText of
      Left _ -> jsonError err400 ("Invalid txOutRef: " <> outRefText)
      Right value -> pure value
  pool <- requireDbPool
  found <- liftIO (DB.MempoolLedger.lookupUtxoByOutRef pool outRef)
  case found of
    Nothing -> jsonError err404 ("UTxO not found for txOutRef " <> outRefText)
    Just utxo ->
      pure $
        Aeson.object
          [ "utxo"
              Aeson..= Aeson.object
                [ "outref" Aeson..= DB.Hex.encodeHex utxo.outref
                , "outputCbor" Aeson..= DB.Hex.encodeHex utxo.outputCbor
                ]
          ]

getUtxosHandler :: Maybe Text -> AppM Aeson.Value
getUtxosHandler maybeAddress = do
  address <- maybe (jsonError err400 "Invalid address: null") pure maybeAddress
  pool <- requireDbPool
  utxos <- liftIO (DB.MempoolLedger.lookupUtxosByAddress pool address)
  pure $
    Aeson.object
      [ "utxos"
          Aeson..= map
            ( \utxo ->
                Aeson.object
                  [ "outref" Aeson..= DB.Hex.encodeHex utxo.outref
                  , "outputCbor" Aeson..= DB.Hex.encodeHex utxo.outputCbor
                  ]
            )
            utxos
      ]

getTxsHandler :: Maybe Text -> AppM Aeson.Value
getTxsHandler maybeAddress = do
  address <- maybe (jsonError err400 "Invalid address: null") pure maybeAddress
  pool <- requireDbPool
  txs <- liftIO (DB.AddressHistory.lookupAddressTxs pool address)
  pure $
    Aeson.object
      [ "txs" Aeson..= map DB.Hex.encodeHex txs
      ]

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
