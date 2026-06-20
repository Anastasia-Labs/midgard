module Midgard.Node.DB.Transactions (
  lookupTxByHash,
) where

import Cardano.Api qualified as C
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Persist.Class (toPersistValue)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxHash)

lookupTxByHash :: Pool SqlBackend -> TxHash -> IO (Either Text (Maybe (C.Tx C.ConwayEra)))
lookupTxByHash pool txHash = do
  mempoolRows <-
    runDB pool $
      rawSql
        "SELECT tx FROM mempool WHERE tx_id = ? LIMIT 1"
        [toPersistValue txHash]
  case mempoolRows of
    (Single tx : _) -> pure (Just <$> decodeConwayTx tx)
    [] -> do
      immutableRows <-
        runDB pool $
          rawSql
            "SELECT tx FROM immutable WHERE tx_id = ? LIMIT 1"
            [toPersistValue txHash]
      pure $
        case immutableRows of
          (Single tx : _) -> Just <$> decodeConwayTx tx
          [] -> Right Nothing

decodeConwayTx :: ByteString -> Either Text (C.Tx C.ConwayEra)
decodeConwayTx bytes =
  case C.deserialiseFromCBOR (C.AsTx C.AsConwayEra) bytes of
    Left err -> Left ("Invalid Conway transaction CBOR in database: " <> Text.pack (show err))
    Right tx -> Right tx
