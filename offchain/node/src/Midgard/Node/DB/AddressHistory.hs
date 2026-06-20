module Midgard.Node.DB.AddressHistory (
  lookupAddressTxs,
) where

import Cardano.Api qualified as C
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistText), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)

lookupAddressTxs :: Pool SqlBackend -> Text -> IO (Either Text [C.Tx C.ConwayEra])
lookupAddressTxs pool address = do
  rows <-
    runDB pool $
      rawSql
        "SELECT tx FROM (SELECT tx_id, tx FROM mempool UNION SELECT tx_id, tx FROM immutable) AS tx_union INNER JOIN address_history ON tx_union.tx_id = address_history.tx_id WHERE address_history.address = ?"
        [PersistText address]
  pure (traverse decodeConwayTx [tx | Single tx <- rows])

decodeConwayTx :: ByteString -> Either Text (C.Tx C.ConwayEra)
decodeConwayTx bytes =
  case C.deserialiseFromCBOR (C.AsTx C.AsConwayEra) bytes of
    Left err -> Left ("Invalid Conway transaction CBOR in database: " <> Text.pack (show err))
    Right tx -> Right tx
