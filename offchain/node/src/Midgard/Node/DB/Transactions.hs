module Midgard.Node.DB.Transactions (
  lookupTxCborByHash,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistByteString), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxHash, unTxHash)

lookupTxCborByHash :: Pool SqlBackend -> TxHash -> IO (Maybe ByteString)
lookupTxCborByHash pool txHash = do
  mempoolRows <-
    runDB pool $
      rawSql
        "SELECT tx FROM mempool WHERE tx_id = ? LIMIT 1"
        [PersistByteString (unTxHash txHash)]
  case mempoolRows of
    (Single tx : _) -> pure (Just tx)
    [] -> do
      immutableRows <-
        runDB pool $
          rawSql
            "SELECT tx FROM immutable WHERE tx_id = ? LIMIT 1"
            [PersistByteString (unTxHash txHash)]
      pure $
        case immutableRows of
          (Single tx : _) -> Just tx
          [] -> Nothing
