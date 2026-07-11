module Midgard.Node.Server.Api (
  HealthAPI,
  QueryAPI,
  AdminAPI,
  TxAPI,
  MidgardNodeAPI,
) where

import Data.Text (Text)
import Midgard.Node.Server.Types (
  BlockResponse,
  DepositBuildRequest,
  DepositBuildResponse,
  DepositStatusResponse,
  HealthResponse,
  MessageResponse,
  ProtocolInfoResponse,
  ReadinessResponse,
  StateQueueResponse,
  SubmitAcceptedResponse,
  SubmitTxRequest,
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
    :<|> "readyz" :> Get '[JSON] ReadinessResponse

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
  "init" :> Get '[JSON] MessageResponse
    :<|> "commit" :> Get '[JSON] MessageResponse
    :<|> "merge" :> Get '[JSON] MessageResponse
    :<|> "stateQueue" :> Get '[JSON] StateQueueResponse
    :<|> "logBlocksDB" :> Get '[JSON] MessageResponse
    :<|> "logGlobals" :> Get '[JSON] MessageResponse

type TxAPI =
  "deposit" :> "build" :> ReqBody '[JSON] DepositBuildRequest :> Post '[JSON] DepositBuildResponse
    :<|> "submit" :> ReqBody '[JSON] SubmitTxRequest :> Post '[JSON] SubmitAcceptedResponse

type MidgardNodeAPI = HealthAPI :<|> QueryAPI :<|> AdminAPI :<|> TxAPI
