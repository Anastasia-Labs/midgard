module Midgard.Node.DB.MempoolLedger (
  StoredUtxo (..),
  lookupUtxoByOutRef,
  lookupUtxosByAddress,
  lookupUtxosByOutRefs,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Class (toPersistValue)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistText), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxOutRefCbor)

data StoredUtxo = StoredUtxo
  { outref :: ByteString
  , outputCbor :: ByteString
  }
  deriving stock (Eq, Show)

lookupUtxoByOutRef :: Pool SqlBackend -> TxOutRefCbor -> IO (Maybe StoredUtxo)
lookupUtxoByOutRef pool rawOutRef = do
  rows <-
    runDB pool $
      rawSql
        "SELECT outref, output FROM mempool_ledger WHERE outref = ? LIMIT 1"
        [toPersistValue rawOutRef]
  pure $
    case rows of
      [] -> Nothing
      ((Single outref, Single output) : _) -> Just (StoredUtxo outref output)

lookupUtxosByAddress :: Pool SqlBackend -> Text -> IO [StoredUtxo]
lookupUtxosByAddress pool address = do
  rows <-
    runDB pool $
      rawSql
        "SELECT outref, output FROM mempool_ledger WHERE address = ?"
        [PersistText address]
  pure [StoredUtxo outref output | (Single outref, Single output) <- rows]

lookupUtxosByOutRefs :: Pool SqlBackend -> [TxOutRefCbor] -> IO [StoredUtxo]
lookupUtxosByOutRefs _ [] = pure []
lookupUtxosByOutRefs pool outRefs =
  fmap foldrKeepOrder (mapM (lookupUtxoByOutRef pool) outRefs)
  where
    foldrKeepOrder = foldr (\entry acc -> maybe acc (: acc) entry) []
