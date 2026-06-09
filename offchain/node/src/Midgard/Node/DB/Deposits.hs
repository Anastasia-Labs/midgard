module Midgard.Node.DB.Deposits (
  DepositStatus (..),
  lookupDepositByEventId,
  lookupDepositsByCardanoTxHash,
) where

import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql (SqlBackend)
import Database.Persist.Sql (PersistValue (PersistByteString), Single (..), rawSql)
import Midgard.Node.DB.Pool (runDB)
import Midgard.Node.DB.Types (TxHash, TxOutRefCbor, unTxHash, unTxOutRefCbor)

data DepositStatus = DepositStatus
  { eventId :: ByteString
  , eventInfo :: ByteString
  , inclusionTime :: UTCTime
  , cardanoTxHash :: ByteString
  , ledgerTxId :: ByteString
  , ledgerOutput :: ByteString
  , ledgerAddress :: Text
  , projectedHeaderHash :: Maybe ByteString
  , status :: Text
  }
  deriving stock (Eq, Show)

lookupDepositByEventId :: Pool SqlBackend -> TxOutRefCbor -> IO (Maybe DepositStatus)
lookupDepositByEventId pool eventId = do
  rows <-
    runDB pool $
      rawSql depositStatusSelect [PersistByteString (unTxOutRefCbor eventId)]
  pure (firstDepositStatus (map decodeDepositStatusRow rows))

lookupDepositsByCardanoTxHash :: Pool SqlBackend -> TxHash -> IO [DepositStatus]
lookupDepositsByCardanoTxHash pool txHash = do
  rows <-
    runDB pool $
      rawSql
        ( depositStatusSelectBase
            <> " WHERE deposit_l1_tx_hash = ? ORDER BY inclusion_time ASC, event_id ASC"
        )
        [PersistByteString (unTxHash txHash)]
  pure (map decodeDepositStatusRow rows)

depositStatusSelect :: Text
depositStatusSelect =
  depositStatusSelectBase <> " WHERE event_id = ? LIMIT 1"

depositStatusSelectBase :: Text
depositStatusSelectBase =
  "SELECT event_id, event_info, inclusion_time, deposit_l1_tx_hash, ledger_tx_id, ledger_output, ledger_address, projected_header_hash, status FROM deposits_utxos"

firstDepositStatus :: [DepositStatus] -> Maybe DepositStatus
firstDepositStatus = \case
  [] -> Nothing
  row : _ -> Just row

decodeDepositStatusRow ::
  ( Single ByteString
  , Single ByteString
  , Single UTCTime
  , Single ByteString
  , Single ByteString
  , Single ByteString
  , Single Text
  , Single (Maybe ByteString)
  , Single Text
  ) ->
  DepositStatus
decodeDepositStatusRow
  ( Single eventId
    , Single eventInfo
    , Single inclusionTime
    , Single cardanoTxHash
    , Single ledgerTxId
    , Single ledgerOutput
    , Single ledgerAddress
    , Single projectedHeaderHash
    , Single status
    ) =
    DepositStatus
      { eventId
      , eventInfo
      , inclusionTime
      , cardanoTxHash
      , ledgerTxId
      , ledgerOutput
      , ledgerAddress
      , projectedHeaderHash
      , status
      }
