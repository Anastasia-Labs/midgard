module Midgard.Node.DB.AddressHistory (
  lookupAddressTxs,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistText), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)

lookupAddressTxs :: Pool SqlBackend -> Text -> IO [ByteString]
lookupAddressTxs pool address = do
  rows <-
    runDB pool $
      rawSql
        "SELECT tx FROM (SELECT tx_id, tx FROM mempool UNION SELECT tx_id, tx FROM immutable) AS tx_union INNER JOIN address_history ON tx_union.tx_id = address_history.tx_id WHERE address_history.address = ?"
        [PersistText address]
  pure [tx | Single tx <- rows]
