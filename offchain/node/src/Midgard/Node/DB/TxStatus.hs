module Midgard.Node.DB.TxStatus (
  TxAdmissionStatus (..),
  TxRejection (..),
  TxStatusFacts (..),
  lookupTxStatusFacts,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistByteString), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxHash, unTxHash)

newtype TxAdmissionStatus = TxAdmissionStatus
  { status :: Text
  }
  deriving stock (Eq, Show)

data TxRejection = TxRejection
  { rejectCode :: Text
  , rejectDetail :: Maybe Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show)

data TxStatusFacts = TxStatusFacts
  { admissionStatus :: Maybe TxAdmissionStatus
  , rejection :: Maybe TxRejection
  , inImmutable :: Bool
  , inMempool :: Bool
  , inProcessedMempool :: Bool
  }
  deriving stock (Eq, Show)

lookupTxStatusFacts :: Pool SqlBackend -> TxHash -> IO TxStatusFacts
lookupTxStatusFacts pool txHash = do
  let rawHash = unTxHash txHash
  admissionStatus <- lookupAdmissionStatus pool rawHash
  rejection <- lookupLatestRejection pool rawHash
  inImmutable <- existsTxId pool "immutable" rawHash
  inMempool <- existsTxId pool "mempool" rawHash
  inProcessedMempool <- existsTxId pool "processed_mempool" rawHash
  pure
    TxStatusFacts
      { admissionStatus
      , rejection
      , inImmutable
      , inMempool
      , inProcessedMempool
      }

lookupAdmissionStatus :: Pool SqlBackend -> ByteString -> IO (Maybe TxAdmissionStatus)
lookupAdmissionStatus pool txHash = do
  rows <-
    runDB pool $
      rawSql
        "SELECT status FROM tx_admissions WHERE tx_id = ? LIMIT 1"
        [PersistByteString txHash]
  pure $
    case rows of
      [] -> Nothing
      Single status : _ -> Just (TxAdmissionStatus status)

lookupLatestRejection :: Pool SqlBackend -> ByteString -> IO (Maybe TxRejection)
lookupLatestRejection pool txHash = do
  rows <-
    runDB pool $
      rawSql
        "SELECT reject_code, reject_detail, created_at FROM tx_rejections WHERE tx_id = ? ORDER BY created_at DESC LIMIT 1"
        [PersistByteString txHash]
  pure $
    case rows of
      [] -> Nothing
      (Single rejectCode, Single rejectDetail, Single createdAt) : _ ->
        Just (TxRejection rejectCode rejectDetail createdAt)

existsTxId :: Pool SqlBackend -> Text -> ByteString -> IO Bool
existsTxId pool tableName txHash = do
  rows <-
    runDB pool $
      rawSql
        ("SELECT 1 FROM " <> tableName <> " WHERE tx_id = ? LIMIT 1")
        [PersistByteString txHash]
  pure $
    case rows :: [Single Int] of
      [] -> False
      _ -> True
