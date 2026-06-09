module Midgard.Node.DB.Blocks (
  lookupBlockTxHashes,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistByteString), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (HeaderHash, unHeaderHash)

lookupBlockTxHashes :: Pool SqlBackend -> HeaderHash -> IO [ByteString]
lookupBlockTxHashes pool headerHash = do
  rows <-
    runDB pool $
      rawSql
        "SELECT tx_id FROM blocks WHERE header_hash = ?"
        [PersistByteString (unHeaderHash headerHash)]
  pure [txHash | Single txHash <- rows]
