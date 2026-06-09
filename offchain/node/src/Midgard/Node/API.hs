module Midgard.Node.API (
  HealthAPI,
  QueryAPI,
  AdminAPI,
  TxAPI,
  MidgardNodeAPI,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Midgard.Node.API.Types (
  BlockResponse,
  DepositStatusResponse,
  HealthResponse,
  PlaceholderResponse,
  PlaceholderWithRequestResponse,
  ProtocolInfoResponse,
  ReadyResponse,
  TxResponse,
  TxStatusResponse,
  TxsResponse,
  UtxoResponse,
  UtxosResponse,
 )
import Servant.API (
  Get,
  JSON,
  Post,
  QueryParam,
  ReqBody,
  type (:<|>) (..),
  type (:>),
 )

type HealthAPI =
  "healthz" :> Get '[JSON] HealthResponse
    :<|> "readyz" :> Get '[JSON] ReadyResponse

type QueryAPI =
  "protocol-info" :> Get '[JSON] ProtocolInfoResponse
    :<|> "tx" :> QueryParam "tx_hash" Text :> Get '[JSON] TxResponse
    :<|> "tx-status" :> QueryParam "tx_hash" Text :> Get '[JSON] TxStatusResponse
    :<|> "deposit-status" :> QueryParam "eventId" Text :> QueryParam "cardanoTxHash" Text :> Get '[JSON] DepositStatusResponse
    :<|> "txs" :> QueryParam "address" Text :> Get '[JSON] TxsResponse
    :<|> "utxo" :> QueryParam "txOutRef" Text :> Get '[JSON] UtxoResponse
    :<|> "utxos" :> QueryParam "address" Text :> Get '[JSON] UtxosResponse
    :<|> "utxos" :> QueryParam "by-outrefs" Text :> ReqBody '[JSON] [Text] :> Post '[JSON] UtxosResponse
    :<|> "block" :> QueryParam "header_hash" Text :> Get '[JSON] BlockResponse

type AdminAPI =
  "init" :> Get '[JSON] PlaceholderResponse
    :<|> "commit" :> Get '[JSON] PlaceholderResponse
    :<|> "merge" :> Get '[JSON] PlaceholderResponse
    :<|> "stateQueue" :> Get '[JSON] PlaceholderResponse
    :<|> "logBlocksDB" :> Get '[JSON] PlaceholderResponse
    :<|> "logGlobals" :> Get '[JSON] PlaceholderResponse

type TxAPI =
  "deposit" :> "build" :> ReqBody '[JSON] Value :> Post '[JSON] PlaceholderWithRequestResponse
    :<|> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] PlaceholderWithRequestResponse

type MidgardNodeAPI = HealthAPI :<|> QueryAPI :<|> AdminAPI :<|> TxAPI
