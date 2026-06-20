module Midgard.Node.Server.Query (
  queryServer,
) where

import Data.Text (Text)
import Midgard.Node.Server.Api (QueryAPI)
import Midgard.Node.Server.Monad (ServerM)
import Midgard.Node.Server.Protocol (protocolInfoHandler)
import Midgard.Node.Server.Query.Types (
  BlockResponse,
  DepositStatusResponse,
  TxResponse,
  TxStatusResponse,
  TxsResponse,
  UtxoResponse,
  UtxosResponse,
 )
import Midgard.Node.Server.Utils (jsonError)
import Servant (ServerT, err501, type (:<|>) (..))

queryServer :: ServerT QueryAPI ServerM
queryServer =
  protocolInfoHandler
    :<|> getTxPlaceholder
    :<|> getTxStatusPlaceholder
    :<|> getDepositStatusPlaceholder
    :<|> getTxsPlaceholder
    :<|> getUtxoPlaceholder
    :<|> getUtxosPlaceholder
    :<|> postUtxosByOutRefsPlaceholder
    :<|> getBlockPlaceholder

getTxPlaceholder :: Maybe Text -> ServerM TxResponse
getTxPlaceholder _ =
  dbPlaceholder "GET /tx is temporarily disabled while DB integration is parked."

getTxStatusPlaceholder :: Maybe Text -> ServerM TxStatusResponse
getTxStatusPlaceholder _ =
  dbPlaceholder "GET /tx-status is temporarily disabled while DB integration is parked."

getDepositStatusPlaceholder :: Maybe Text -> Maybe Text -> ServerM DepositStatusResponse
getDepositStatusPlaceholder _ _ =
  dbPlaceholder "GET /deposit-status is temporarily disabled while DB integration is parked."

getTxsPlaceholder :: Maybe Text -> ServerM TxsResponse
getTxsPlaceholder _ =
  dbPlaceholder "GET /txs is temporarily disabled while DB integration is parked."

getUtxoPlaceholder :: Maybe Text -> ServerM UtxoResponse
getUtxoPlaceholder _ =
  dbPlaceholder "GET /utxo is temporarily disabled while DB integration is parked."

getUtxosPlaceholder :: Maybe Text -> ServerM UtxosResponse
getUtxosPlaceholder _ =
  dbPlaceholder "GET /utxos is temporarily disabled while DB integration is parked."

postUtxosByOutRefsPlaceholder :: Maybe Text -> [Text] -> ServerM UtxosResponse
postUtxosByOutRefsPlaceholder _ _ =
  dbPlaceholder "POST /utxos?by-outrefs is temporarily disabled while DB integration is parked."

getBlockPlaceholder :: Maybe Text -> ServerM BlockResponse
getBlockPlaceholder _ =
  dbPlaceholder "GET /block is temporarily disabled while DB integration is parked."

dbPlaceholder :: Text -> ServerM a
dbPlaceholder =
  -- TODO(db): restore these handlers when the DB/indexer sublibrary is ready.
  jsonError err501
