module Spec.Node.DB.Utils (
  blob,
  headerHashBlob,
  hexText,
  outRefBlob,
  seedAddressHistoryRow,
  seedBlockRow,
  seedDepositRow,
  seedImmutableRow,
  seedLedgerRow,
  seedMempoolRow,
  seedProcessedMempoolRow,
  seedTxAdmissionRow,
  seedTxRejectionRow,
  testHeaderHash,
  testTime,
  testTxHash,
  testTxOutRef,
  txHashBlob,
  withTestPool,
) where

import Control.Exception (bracket)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Word (Word8)
import Database.Persist.Sql (PersistValue (PersistByteString, PersistNull, PersistText, PersistUTCTime), SqlBackend, rawExecute)
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Midgard.Node.DB.Deposits (DepositStatus (..))
import Midgard.Node.DB.Hex qualified as DB.Hex
import Midgard.Node.DB.Types qualified as DB.Types
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

withTestPool :: (Pool SqlBackend -> IO a) -> IO a
withTestPool action =
  bracket acquire release action'
  where
    acquire = do
      dbPath <- freshDbPath
      pool <- runNoLoggingT (createSqlitePool (Text.pack dbPath) 1)
      initialiseSchema pool
      pure (dbPath, pool)
    release (dbPath, pool) = do
      destroyAllResources pool
      removeFile dbPath
    action' (_, pool) = action pool

{- | The query modules are backend-agnostic raw SQL, so SQLite gives us a fast
local harness for seeding deterministic mock rows and validating result
decoding without needing a live YugaByte instance.
-}
initialiseSchema :: Pool SqlBackend -> IO ()
initialiseSchema pool =
  runSqlPool setup pool
  where
    setup = do
      rawExecute "CREATE TABLE mempool (tx_id BLOB PRIMARY KEY, tx BLOB NOT NULL)" []
      rawExecute "CREATE TABLE immutable (tx_id BLOB PRIMARY KEY, tx BLOB NOT NULL)" []
      rawExecute "CREATE TABLE blocks (header_hash BLOB NOT NULL, tx_id BLOB NOT NULL)" []
      rawExecute "CREATE TABLE mempool_ledger (outref BLOB PRIMARY KEY, output BLOB NOT NULL, address TEXT NOT NULL)" []
      rawExecute "CREATE TABLE address_history (address TEXT NOT NULL, tx_id BLOB NOT NULL)" []
      rawExecute "CREATE TABLE processed_mempool (tx_id BLOB PRIMARY KEY, tx BLOB NOT NULL)" []
      rawExecute "CREATE TABLE tx_admissions (tx_id BLOB PRIMARY KEY, status TEXT NOT NULL)" []
      rawExecute "CREATE TABLE tx_rejections (tx_id BLOB NOT NULL, reject_code TEXT NOT NULL, reject_detail TEXT, created_at TIMESTAMP NOT NULL)" []
      rawExecute "CREATE TABLE deposits_utxos (event_id BLOB PRIMARY KEY, event_info BLOB NOT NULL, inclusion_time TIMESTAMP NOT NULL, deposit_l1_tx_hash BLOB NOT NULL, ledger_tx_id BLOB NOT NULL, ledger_output BLOB NOT NULL, ledger_address TEXT NOT NULL, projected_header_hash BLOB, status TEXT NOT NULL)" []

seedMempoolRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedMempoolRow pool txHash tx =
  runSqlPool (rawExecute "INSERT INTO mempool (tx_id, tx) VALUES (?, ?)" [toPersistBlob txHash, toPersistBlob tx]) pool

seedImmutableRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedImmutableRow pool txHash tx =
  runSqlPool (rawExecute "INSERT INTO immutable (tx_id, tx) VALUES (?, ?)" [toPersistBlob txHash, toPersistBlob tx]) pool

seedBlockRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedBlockRow pool headerHash txHash =
  runSqlPool (rawExecute "INSERT INTO blocks (header_hash, tx_id) VALUES (?, ?)" [toPersistBlob headerHash, toPersistBlob txHash]) pool

seedLedgerRow :: Pool SqlBackend -> ByteString -> Text -> ByteString -> IO ()
seedLedgerRow pool outref address output =
  runSqlPool
    ( rawExecute
        "INSERT INTO mempool_ledger (outref, output, address) VALUES (?, ?, ?)"
        [toPersistBlob outref, toPersistBlob output, toPersistText address]
    )
    pool

seedAddressHistoryRow :: Pool SqlBackend -> Text -> ByteString -> IO ()
seedAddressHistoryRow pool address txHash =
  runSqlPool
    (rawExecute "INSERT INTO address_history (address, tx_id) VALUES (?, ?)" [toPersistText address, toPersistBlob txHash])
    pool

seedProcessedMempoolRow :: Pool SqlBackend -> ByteString -> ByteString -> IO ()
seedProcessedMempoolRow pool txHash tx =
  runSqlPool (rawExecute "INSERT INTO processed_mempool (tx_id, tx) VALUES (?, ?)" [toPersistBlob txHash, toPersistBlob tx]) pool

seedTxAdmissionRow :: Pool SqlBackend -> ByteString -> Text -> IO ()
seedTxAdmissionRow pool txHash status =
  runSqlPool (rawExecute "INSERT INTO tx_admissions (tx_id, status) VALUES (?, ?)" [toPersistBlob txHash, toPersistText status]) pool

seedTxRejectionRow :: Pool SqlBackend -> ByteString -> Text -> Maybe Text -> UTCTime -> IO ()
seedTxRejectionRow pool txHash rejectCode rejectDetail createdAt =
  runSqlPool
    ( rawExecute
        "INSERT INTO tx_rejections (tx_id, reject_code, reject_detail, created_at) VALUES (?, ?, ?, ?)"
        [toPersistBlob txHash, toPersistText rejectCode, maybe PersistNull toPersistText rejectDetail, toPersistTime createdAt]
    )
    pool

seedDepositRow :: Pool SqlBackend -> DepositStatus -> IO ()
seedDepositRow pool deposit =
  runSqlPool
    ( rawExecute
        "INSERT INTO deposits_utxos (event_id, event_info, inclusion_time, deposit_l1_tx_hash, ledger_tx_id, ledger_output, ledger_address, projected_header_hash, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        [ toPersistBlob deposit.eventId
        , toPersistBlob deposit.eventInfo
        , toPersistTime deposit.inclusionTime
        , toPersistBlob deposit.cardanoTxHash
        , toPersistBlob deposit.ledgerTxId
        , toPersistBlob deposit.ledgerOutput
        , toPersistText deposit.ledgerAddress
        , maybe PersistNull toPersistBlob deposit.projectedHeaderHash
        , toPersistText deposit.status
        ]
    )
    pool

toPersistBlob :: ByteString -> PersistValue
toPersistBlob = PersistByteString

toPersistText :: Text -> PersistValue
toPersistText = PersistText

toPersistTime :: UTCTime -> PersistValue
toPersistTime = PersistUTCTime

blob :: Word8 -> ByteString
blob byte = ByteString.pack [byte, byte + 1, byte + 2]

txHashBlob :: Word8 -> ByteString
txHashBlob byte = deterministicBlob 32 byte

headerHashBlob :: Word8 -> ByteString
headerHashBlob byte = deterministicBlob 28 byte

outRefBlob :: Word8 -> ByteString
outRefBlob byte = deterministicBlob 10 byte

deterministicBlob :: Int -> Word8 -> ByteString
deterministicBlob len seed =
  ByteString.pack [seed + fromIntegral offset | offset <- [0 .. len - 1]]

testTxHash :: Word8 -> DB.Types.TxHash
testTxHash byte =
  either (error . Text.unpack) id (DB.Types.mkTxHash (txHashBlob byte))

testHeaderHash :: Word8 -> DB.Types.HeaderHash
testHeaderHash byte =
  either (error . Text.unpack) id (DB.Types.mkHeaderHash (headerHashBlob byte))

testTxOutRef :: Word8 -> DB.Types.TxOutRefCbor
testTxOutRef byte =
  either (error . Text.unpack) id (DB.Types.mkTxOutRefCbor (outRefBlob byte))

hexText :: ByteString -> Text
hexText = DB.Hex.encodeHex

testTime :: Integer -> UTCTime
testTime seconds =
  UTCTime (fromGregorian 2026 6 9) (secondsToDiffTime seconds)

freshDbPath :: IO FilePath
freshDbPath = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir "midgard-node-db-tests.sqlite"
  hClose handle
  pure path
