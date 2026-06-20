module Midgard.Node.DB.TxStatus (
  TxAdmissionStatus (..),
  TxRejection (..),
  TxStatusFacts (..),
  lookupTxStatusFacts,
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Class (toPersistValue)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxHash)

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
  admissionStatus <- lookupAdmissionStatus pool txHash
  rejection <- lookupLatestRejection pool txHash
  inImmutable <- existsTxId pool "immutable" txHash
  inMempool <- existsTxId pool "mempool" txHash
  inProcessedMempool <- existsTxId pool "processed_mempool" txHash
  pure
    TxStatusFacts
      { admissionStatus
      , rejection
      , inImmutable
      , inMempool
      , inProcessedMempool
      }

lookupAdmissionStatus :: Pool SqlBackend -> TxHash -> IO (Maybe TxAdmissionStatus)
lookupAdmissionStatus pool txHash = do
  rows <-
    runDB pool $
      rawSql
        "SELECT status FROM tx_admissions WHERE tx_id = ? LIMIT 1"
        [toPersistValue txHash]
  pure $
    case rows of
      [] -> Nothing
      Single status : _ -> Just (TxAdmissionStatus status)

lookupLatestRejection :: Pool SqlBackend -> TxHash -> IO (Maybe TxRejection)
lookupLatestRejection pool txHash = do
  rows <-
    runDB pool $
      rawSql
        "SELECT reject_code, reject_detail, created_at FROM tx_rejections WHERE tx_id = ? ORDER BY created_at DESC LIMIT 1"
        [toPersistValue txHash]
  pure $
    case rows of
      [] -> Nothing
      (Single rejectCode, Single rejectDetail, Single createdAt) : _ ->
        Just (TxRejection rejectCode rejectDetail createdAt)

existsTxId :: Pool SqlBackend -> Text -> TxHash -> IO Bool
existsTxId pool tableName txHash = do
  rows <-
    runDB pool $
      rawSql
        ("SELECT 1 FROM " <> tableName <> " WHERE tx_id = ? LIMIT 1")
        [toPersistValue txHash]
  pure $
    case rows :: [Single Int] of
      [] -> False
      _ -> True
