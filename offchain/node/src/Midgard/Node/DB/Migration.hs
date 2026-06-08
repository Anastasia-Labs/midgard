module Midgard.Node.DB.Migration (
  Migration (..),
  MigrationStatus (..),
  migrateDatabase,
  verifyDatabase,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.List (find, sortOn)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (
  PersistValue (PersistInt64, PersistText),
  Single (..),
  rawExecute,
  rawSql,
 )
import GHC.Generics (Generic)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.Migrations qualified as Migrations
import System.FilePath (takeBaseName)
import Text.Read (readMaybe)

-- Runtime representation of the TypeScript-owned SQL migrations.
-- We intentionally do not duplicate the schema in Haskell types yet; the SQL
-- files remain the source of truth for both implementations.
data Migration = Migration
  { version :: Int
  , name :: Text
  , checksumSha256 :: Text
  , sql :: Text
  , path :: FilePath
  }
  deriving stock (Eq, Generic, Show)

data AppliedMigration = AppliedMigration
  { version :: Int
  , name :: Text
  , checksumSha256 :: Text
  }
  deriving stock (Eq, Generic, Show)

data MigrationStatus = MigrationStatus
  { expectedVersion :: Int
  , actualVersion :: Int
  , compatible :: Bool
  , missingTables :: [Text]
  , missingIndexes :: [Text]
  }
  deriving stock (Eq, Generic, Show)

data MigrationError
  = InvalidMigrationFilename FilePath
  | SchemaVersionMismatch Int Int
  | MigrationChecksumMismatch Int
  | MissingTables [Text]
  | MissingIndexes [Text]
  deriving stock (Eq, Show)

instance Exception MigrationError

-- These are copied from the TypeScript migration manifest and let `verify`
-- assert not just the version ledger but also the concrete application shape.
applicationTableNames :: [Text]
applicationTableNames =
  [ "address_history"
  , "blocks"
  , "confirmed_ledger"
  , "latest_ledger"
  , "deposits_utxos"
  , "withdrawal_utxos"
  , "immutable"
  , "mempool"
  , "processed_mempool"
  , "mempool_ledger"
  , "mempool_tx_deltas"
  , "tx_rejections"
  , "deposit_ingestion_cursor"
  , "pending_block_finalizations"
  , "pending_block_finalization_deposits"
  , "pending_block_finalization_withdrawals"
  , "pending_block_finalization_txs"
  , "tx_admissions"
  , "local_mutation_jobs"
  , "state_queue_mutation_leases"
  ]

applicationIndexNames :: [Text]
applicationIndexNames =
  [ "idx_address_history_created_at"
  , "idx_blocks_header_hash"
  , "idx_blocks_tx_id"
  , "idx_confirmed_ledger_address"
  , "idx_latest_ledger_address"
  , "idx_deposits_utxos_status_inclusion_time_event_id"
  , "idx_deposits_utxos_projected_header_hash"
  , "idx_deposits_utxos_deposit_l1_tx_hash"
  , "idx_withdrawal_utxos_status_inclusion_time_event_id"
  , "idx_withdrawal_utxos_projected_header_hash"
  , "idx_withdrawal_utxos_withdrawal_l1_tx_hash"
  , "idx_withdrawal_utxos_l2_outref"
  , "idx_immutable_time_stamp_tz"
  , "idx_mempool_time_stamp_tz"
  , "idx_processed_mempool_time_stamp_tz"
  , "idx_mempool_ledger_address"
  , "idx_mempool_ledger_source_event_id"
  , "uniq_mempool_ledger_source_event_id"
  , "idx_tx_rejections_tx_id"
  , "idx_tx_rejections_created_at"
  , "uniq_pending_block_finalizations_single_active"
  , "idx_pending_block_finalizations_status"
  , "idx_tx_admissions_dequeue"
  , "idx_tx_admissions_status_updated"
  , "idx_tx_admissions_lease"
  , "uniq_tx_rejections_tx_id"
  , "idx_local_mutation_jobs_status_updated"
  , "uniq_state_queue_mutation_leases_active_scope"
  , "idx_state_queue_mutation_leases_status_updated"
  ]

loadMigrations :: IO [Migration]
loadMigrations = do
  paths <- Migrations.listSqlMigrations
  sortOn (.version) <$> traverse loadMigration paths

-- Infer version and logical name from filenames like
-- `0003_local_mutation_jobs.sql`.
loadMigration :: FilePath -> IO Migration
loadMigration migrationPath = do
  sqlBytes <- LBS.readFile migrationPath
  let baseName = takeBaseName migrationPath
      (rawVersion, rawName) = break (== '_') baseName
      versionText = dropWhile (== '0') rawVersion
      parsedVersion = readMaybe (if null versionText then "0" else versionText)
      migrationName = drop 1 rawName
  case parsedVersion of
    Nothing -> throwIO (InvalidMigrationFilename migrationPath)
    Just migrationVersion ->
      pure
        Migration
          { version = migrationVersion
          , name = Text.pack migrationName
          , checksumSha256 = sha256Hex sqlBytes
          , sql = Text.decodeUtf8 (LBS.toStrict sqlBytes)
          , path = migrationPath
          }

sha256Hex :: LBS.ByteString -> Text
sha256Hex =
  Text.decodeUtf8
    . B16.encode
    . SHA256.hashlazy

ensureMetadataTables :: Pool SqlBackend -> IO ()
ensureMetadataTables pool = do
  -- Keep metadata schema intentionally small for the first pass.
  -- We can add richer audit/event payloads once the basic migration flow has
  -- been proven against a real YugaByte deployment.
  runDB pool $
    rawExecute
      "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY CHECK (version > 0), name TEXT NOT NULL, checksum_sha256 TEXT NOT NULL, applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW())"
      []
  runDB pool $
    rawExecute
      "CREATE TABLE IF NOT EXISTS schema_migration_events (id BIGSERIAL PRIMARY KEY, version INTEGER, name TEXT, checksum_sha256 TEXT, event_type TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), details JSONB NOT NULL DEFAULT '{}'::jsonb)"
      []

readAppliedMigrations :: Pool SqlBackend -> IO [AppliedMigration]
readAppliedMigrations pool = do
  rows <-
    runDB pool $
      rawSql
        "SELECT version, name, checksum_sha256 FROM schema_migrations ORDER BY version ASC"
        []
  pure
    [ AppliedMigration
        { version = version'
        , name = name'
        , checksumSha256 = checksum'
        }
    | (Single version', Single name', Single checksum') <- rows
    ]

readExistingTables :: Pool SqlBackend -> IO [Text]
readExistingTables pool = do
  rows <-
    runDB pool $
      rawSql
        "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE' ORDER BY table_name ASC"
        []
  pure [tableName | Single tableName <- rows]

readExistingIndexes :: Pool SqlBackend -> IO [Text]
readExistingIndexes pool = do
  rows <-
    runDB pool $
      rawSql
        "SELECT indexname FROM pg_indexes WHERE schemaname = 'public' ORDER BY indexname ASC"
        []
  pure [indexName | Single indexName <- rows]

recordMigrationEvent :: Pool SqlBackend -> Maybe Migration -> Text -> IO ()
recordMigrationEvent pool maybeMigration eventType =
  runDB pool $
    rawExecute
      "INSERT INTO schema_migration_events (version, name, checksum_sha256, event_type, details) VALUES (?, ?, ?, ?, '{}'::jsonb)"
      [ maybe (PersistInt64 0) (PersistInt64 . intToInt64 . (.version)) maybeMigration
      , maybe (PersistText "") (PersistText . (.name)) maybeMigration
      , maybe (PersistText "") (PersistText . (.checksumSha256)) maybeMigration
      , PersistText eventType
      ]

applyMigration :: Pool SqlBackend -> Migration -> IO ()
applyMigration pool migration = do
  recordMigrationEvent pool (Just migration) "started"
  -- Each SQL file is executed as-is so the DB behavior stays aligned with the
  -- TypeScript node's schema assumptions.
  runDB pool $ rawExecute migration.sql []
  runDB pool $
    rawExecute
      "INSERT INTO schema_migrations (version, name, checksum_sha256) VALUES (?, ?, ?)"
      [ PersistInt64 (intToInt64 migration.version)
      , PersistText migration.name
      , PersistText migration.checksumSha256
      ]
  recordMigrationEvent pool (Just migration) "succeeded"

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

validateAppliedMigrations :: [Migration] -> [AppliedMigration] -> IO ()
validateAppliedMigrations expected applied =
  forM_ applied $ \row ->
    case find (\migration -> migration.version == row.version) expected of
      Nothing -> throwIO (SchemaVersionMismatch row.version (lastExpectedVersion expected))
      Just migration ->
        when (migration.checksumSha256 /= row.checksumSha256) $
          throwIO (MigrationChecksumMismatch row.version)

missingMembers :: [Text] -> [Text] -> [Text]
missingMembers expected actual = filter (`notElem` actual) expected

lastExpectedVersion :: [Migration] -> Int
lastExpectedVersion [] = 0
lastExpectedVersion migrations = maximum (map (.version) migrations)

migrateDatabase :: Pool SqlBackend -> IO ()
migrateDatabase pool = do
  ensureMetadataTables pool
  expected <- loadMigrations
  applied <- readAppliedMigrations pool
  validateAppliedMigrations expected applied
  let appliedVersions = map (.version) applied
      pending = filter (\migration -> migration.version `notElem` appliedVersions) expected
  -- We deliberately do not auto-repair or rewrite already-applied migrations:
  -- drift should fail loudly so we notice incompatible schema history.
  forM_ pending (applyMigration pool)

verifyDatabase :: Pool SqlBackend -> IO MigrationStatus
verifyDatabase pool = do
  ensureMetadataTables pool
  expected <- loadMigrations
  applied <- readAppliedMigrations pool
  validateAppliedMigrations expected applied
  let expectedVersion' = lastExpectedVersion expected
      actualVersion' =
        if null applied
          then 0
          else maximum (map (.version) applied)
  unless (actualVersion' == expectedVersion') $
    throwIO (SchemaVersionMismatch expectedVersion' actualVersion')
  existingTables <- readExistingTables pool
  existingIndexes <- readExistingIndexes pool
  let missingTables' = missingMembers applicationTableNames existingTables
      missingIndexes' = missingMembers applicationIndexNames existingIndexes
  unless (null missingTables') $
    throwIO (MissingTables missingTables')
  unless (null missingIndexes') $
    throwIO (MissingIndexes missingIndexes')
  pure
    MigrationStatus
      { expectedVersion = expectedVersion'
      , actualVersion = actualVersion'
      , compatible = True
      , missingTables = missingTables'
      , missingIndexes = missingIndexes'
      }
