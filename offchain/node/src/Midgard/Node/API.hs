module Midgard.Node.API (
  HealthAPI,
  QueryAPI,
  AdminAPI,
  TxAPI,
  MidgardNodeAPI,
  UtxoLookupRequest (..),
  HealthResponse (..),
  ReadyResponse (..),
  ProtocolInfoResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (
  Get,
  JSON,
  Post,
  QueryParam,
  ReqBody,
  type (:<|>) (..),
  type (:>),
 )

data UtxoLookupRequest = UtxoLookupRequest
  { txOutRefs :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data HealthResponse = HealthResponse
  { status :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data ReadyResponse = ReadyResponse
  { ready :: Bool
  , reasons :: [Text]
  , dbConfigured :: Bool
  , schemaSource :: FilePath
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data ProtocolInfoResponse = ProtocolInfoResponse
  { network :: Text
  , mutatingEndpointsEnabled :: Bool
  , migrationDirectory :: FilePath
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

type HealthAPI =
  "healthz" :> Get '[JSON] HealthResponse
    :<|> "readyz" :> Get '[JSON] ReadyResponse

type QueryAPI =
  "protocol-info" :> Get '[JSON] ProtocolInfoResponse
    :<|> "tx" :> QueryParam "tx_hash" Text :> Get '[JSON] Value
    :<|> "tx-status" :> QueryParam "tx_hash" Text :> Get '[JSON] Value
    :<|> "deposit-status" :> QueryParam "eventId" Text :> QueryParam "l1TxHash" Text :> Get '[JSON] Value
    :<|> "txs" :> QueryParam "address" Text :> Get '[JSON] Value
    :<|> "utxo" :> QueryParam "txOutRef" Text :> Get '[JSON] Value
    :<|> "utxos" :> QueryParam "address" Text :> Get '[JSON] Value
    :<|> "utxos" :> ReqBody '[JSON] UtxoLookupRequest :> Post '[JSON] Value
    :<|> "block" :> QueryParam "headerHash" Text :> Get '[JSON] Value

type AdminAPI =
  "init" :> Get '[JSON] Value
    :<|> "commit" :> Get '[JSON] Value
    :<|> "merge" :> Get '[JSON] Value
    :<|> "stateQueue" :> Get '[JSON] Value
    :<|> "logBlocksDB" :> Get '[JSON] Value
    :<|> "logGlobals" :> Get '[JSON] Value

type TxAPI =
  "deposit" :> "build" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "submit" :> ReqBody '[JSON] Value :> Post '[JSON] Value

type MidgardNodeAPI = HealthAPI :<|> QueryAPI :<|> AdminAPI :<|> TxAPI
