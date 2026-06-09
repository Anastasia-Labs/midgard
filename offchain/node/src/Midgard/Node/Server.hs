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
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Database.Persist.Postgresql (SqlBackend)
import Midgard.Node.API (
  AdminAPI,
  HealthAPI,
  MidgardNodeAPI,
  QueryAPI,
  TxAPI,
 )
import Midgard.Node.API.Types (
  BlockResponse (..),
  DepositStatusResponse (..),
  EncodedStoredUtxo (..),
  HealthResponse (..),
  PlaceholderResponse (..),
  PlaceholderWithRequestResponse (..),
  ProtocolInfoResponse (..),
  ReadyResponse (..),
  TxResponse (..),
  TxStatusResponse (..),
  TxStatusTimestamps (..),
  TxsResponse (..),
  UtxoResponse (..),
  UtxosResponse (..),
 )
import Midgard.Node.Config qualified as Config
import Midgard.Node.DB.AddressHistory qualified as DB.AddressHistory
import Midgard.Node.DB.Blocks qualified as DB.Blocks
import Midgard.Node.DB.Deposits qualified as DB.Deposits
import Midgard.Node.DB.Health qualified as DB.Health
import Midgard.Node.DB.Hex qualified as DB.Hex
import Midgard.Node.DB.MempoolLedger qualified as DB.MempoolLedger
import Midgard.Node.DB.Migration qualified as DB.Migration
import Midgard.Node.DB.Transactions qualified as DB.Transactions
import Midgard.Node.DB.TxStatus qualified as DB.TxStatus
import Midgard.Node.DB.Types qualified as DB.Types
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Migrations qualified as Migrations
import Midgard.Node.TxOutRef qualified as TxOutRef
import Network.Wai (Application)
import Servant (
  Handler,
  ServerError (..),
  ServerT,
  err400,
  err404,
  err409,
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
    :<|> getTxStatusHandler
    :<|> getDepositStatusHandler
    :<|> getTxsHandler
    :<|> getUtxoHandler
    :<|> getUtxosHandler
    :<|> postUtxosByOutRefsHandler
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
  template {errBody = LBS.fromStrict (TE.encodeUtf8 ("{\"error\":\"" <> message <> "\"}"))}

jsonError :: ServerError -> Text -> AppM a
jsonError template message = throwError (serverErrorWith template message)

getTxHandler :: Maybe Text -> AppM TxResponse
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
    Just txCbor -> pure (TxResponse (DB.Hex.encodeHex txCbor))

getBlockHandler :: Maybe Text -> AppM BlockResponse
getBlockHandler maybeHeaderHash = do
  headerHashText <- maybe (jsonError err400 "Invalid block hash: null") pure maybeHeaderHash
  headerHash <-
    case DB.Hex.decodeHeaderHashHex headerHashText of
      Left _ -> jsonError err400 ("Invalid block hash: " <> headerHashText)
      Right value -> pure value
  pool <- requireDbPool
  hashes <- liftIO (DB.Blocks.lookupBlockTxHashes pool headerHash)
  pure (BlockResponse (map DB.Hex.encodeHex hashes))

getUtxoHandler :: Maybe Text -> AppM UtxoResponse
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
    Just utxo -> pure (UtxoResponse (encodeStoredUtxo utxo))

getUtxosHandler :: Maybe Text -> AppM UtxosResponse
getUtxosHandler maybeAddress = do
  address <- maybe (jsonError err400 "Invalid address: null") pure maybeAddress
  pool <- requireDbPool
  utxos <- liftIO (DB.MempoolLedger.lookupUtxosByAddress pool address)
  pure (UtxosResponse (map encodeStoredUtxo utxos))

getTxsHandler :: Maybe Text -> AppM TxsResponse
getTxsHandler maybeAddress = do
  address <- maybe (jsonError err400 "Invalid address: null") pure maybeAddress
  pool <- requireDbPool
  txs <- liftIO (DB.AddressHistory.lookupAddressTxs pool address)
  pure (TxsResponse (map DB.Hex.encodeHex txs))

encodeStoredUtxo :: DB.MempoolLedger.StoredUtxo -> EncodedStoredUtxo
encodeStoredUtxo utxo =
  EncodedStoredUtxo
    { outref = DB.Hex.encodeHex utxo.outref
    , outputCbor = DB.Hex.encodeHex utxo.outputCbor
    }

getTxStatusHandler :: Maybe Text -> AppM TxStatusResponse
getTxStatusHandler maybeTxHash = do
  txHashText <- maybe (jsonError err400 "Invalid transaction hash: null") pure maybeTxHash
  txHash <-
    case DB.Hex.decodeTxHashHex txHashText of
      Left _ -> jsonError err400 ("Invalid transaction hash: " <> txHashText)
      Right value -> pure value
  pool <- requireDbPool
  facts <- liftIO (DB.TxStatus.lookupTxStatusFacts pool txHash)
  let (status, response) = encodeTxStatus txHashText facts
  if status == "not_found"
    then jsonError err404 ("Transaction status not found for " <> txHashText)
    else pure response

encodeTxStatus :: Text -> DB.TxStatus.TxStatusFacts -> (Text, TxStatusResponse)
encodeTxStatus txId facts
  | facts.inImmutable =
      simpleTxStatus "committed"
  | facts.inProcessedMempool =
      simpleTxStatus "pending_commit"
  | facts.inMempool =
      simpleTxStatus "accepted"
  | Just rejection <- facts.rejection =
      ( "rejected"
      , TxStatusResponse
          { txId
          , status = "rejected"
          , reasonCode = Just rejection.rejectCode
          , reasonDetail = rejection.rejectDetail
          , timestamps = Just (TxStatusTimestamps rejection.createdAt)
          }
      )
  | Just admission <- facts.admissionStatus
  , admission.status == "validating" =
      simpleTxStatus "validating"
  | Just admission <- facts.admissionStatus
  , admission.status == "queued" =
      simpleTxStatus "queued"
  | otherwise =
      simpleTxStatus "not_found"
  where
    simpleTxStatus status =
      ( status
      , TxStatusResponse
          { txId
          , status
          , reasonCode = Nothing
          , reasonDetail = Nothing
          , timestamps = Nothing
          }
      )

getDepositStatusHandler :: Maybe Text -> Maybe Text -> AppM DepositStatusResponse
getDepositStatusHandler maybeEventId maybeCardanoTxHash = do
  case (maybeEventId, maybeCardanoTxHash) of
    (Nothing, Nothing) ->
      jsonError err400 "GET /deposit-status requires `eventId` or `cardanoTxHash`."
    _ -> do
      eventId <- traverse parseEventId maybeEventId
      cardanoTxHash <- traverse parseCardanoTxHash maybeCardanoTxHash
      pool <- requireDbPool
      byEventId <- traverse (liftIO . DB.Deposits.lookupDepositByEventId pool) eventId
      resolveDepositStatus pool maybeEventId maybeCardanoTxHash byEventId cardanoTxHash
  where
    parseEventId value =
      case DB.Hex.decodeTxOutRefHex value of
        Left _ -> jsonError err400 ("Invalid eventId: " <> value)
        Right parsed -> pure parsed
    parseCardanoTxHash value =
      case DB.Hex.decodeTxHashHex value of
        Left _ -> jsonError err400 ("Invalid cardanoTxHash: " <> value)
        Right parsed -> pure parsed

resolveDepositStatus ::
  Pool SqlBackend ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Maybe DB.Deposits.DepositStatus) ->
  Maybe DB.Types.TxHash ->
  AppM DepositStatusResponse
resolveDepositStatus pool maybeEventId maybeCardanoTxHash byEventId maybeCardanoTxHashBytes =
  case (byEventId, maybeCardanoTxHashBytes) of
    (Just Nothing, _) ->
      jsonError err404 ("Deposit not found for eventId " <> maybe "" id maybeEventId)
    (Just (Just deposit), Just cardanoTxHash)
      | deposit.cardanoTxHash /= DB.Types.unTxHash cardanoTxHash ->
          jsonError err409 "Provided eventId and cardanoTxHash do not refer to the same deposit."
      | otherwise ->
          pure (encodeDepositStatus deposit)
    (Just (Just deposit), Nothing) ->
      pure (encodeDepositStatus deposit)
    (Nothing, Just cardanoTxHash) -> do
      matches <- liftIO (DB.Deposits.lookupDepositsByCardanoTxHash pool cardanoTxHash)
      case matches of
        [] -> jsonError err404 ("Deposit not found for cardanoTxHash " <> maybe "" id maybeCardanoTxHash)
        [deposit] -> pure (encodeDepositStatus deposit)
        _ -> jsonError err409 "Multiple deposits found for the provided cardanoTxHash; query by eventId to disambiguate."
    (Nothing, Nothing) ->
      jsonError err400 "GET /deposit-status requires `eventId` or `cardanoTxHash`."

encodeDepositStatus :: DB.Deposits.DepositStatus -> DepositStatusResponse
encodeDepositStatus deposit =
  DepositStatusResponse
    { eventId = DB.Hex.encodeHex deposit.eventId
    , eventInfo = DB.Hex.encodeHex deposit.eventInfo
    , inclusionTime = deposit.inclusionTime
    , cardanoTxHash = DB.Hex.encodeHex deposit.cardanoTxHash
    , ledgerTxId = DB.Hex.encodeHex deposit.ledgerTxId
    , ledgerOutput = DB.Hex.encodeHex deposit.ledgerOutput
    , ledgerAddress = deposit.ledgerAddress
    , projectedHeaderHash = fmap DB.Hex.encodeHex deposit.projectedHeaderHash
    , status = deposit.status
    }

postUtxosByOutRefsHandler :: Maybe Text -> [Text] -> AppM UtxosResponse
postUtxosByOutRefsHandler maybeSelector txOutRefLabels = do
  case maybeSelector of
    Nothing -> jsonError err400 "POST /utxos requires the `?by-outrefs` query selector."
    Just _ -> pure ()
  txOutRefs <- parseRequestedOutRefs txOutRefLabels
  pool <- requireDbPool
  utxos <- liftIO (DB.MempoolLedger.lookupUtxosByOutRefs pool txOutRefs)
  pure (UtxosResponse (map encodeStoredUtxo utxos))

parseRequestedOutRefs :: [Text] -> AppM [DB.Types.TxOutRefCbor]
parseRequestedOutRefs labels =
  go mempty [] (zip [0 :: Int ..] labels)
  where
    go _seen acc [] = pure (reverse acc)
    go seen acc ((index, label) : rest) =
      case TxOutRef.parseTxOutRefLabel label of
        Left err -> jsonError err400 ("txOutRefs[" <> showText index <> "]: " <> err)
        Right outRef
          | DB.Types.unTxOutRefCbor outRef `elem` seen ->
              jsonError err400 ("Duplicate txOutRef provided at txOutRefs[" <> showText index <> "].")
          | otherwise ->
              go (DB.Types.unTxOutRefCbor outRef : seen) (outRef : acc) rest

showText :: (Show a) => a -> Text
showText = Text.pack . show

placeholderJsonBodyHandler :: Text -> Aeson.Value -> AppM PlaceholderWithRequestResponse
placeholderJsonBodyHandler endpoint body =
  pure
    PlaceholderWithRequestResponse
      { endpoint
      , status = "not_implemented"
      , request = body
      }

notImplementedHandler :: Text -> AppM PlaceholderResponse
notImplementedHandler endpoint =
  pure
    PlaceholderResponse
      { endpoint
      , status = "not_implemented"
      }
