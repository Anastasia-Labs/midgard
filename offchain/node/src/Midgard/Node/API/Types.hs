module Midgard.Node.API.Types (
  BlockResponse (..),
  DepositStatusResponse (..),
  EncodedStoredUtxo (..),
  HealthResponse (..),
  PlaceholderResponse (..),
  PlaceholderWithRequestResponse (..),
  ProtocolInfoResponse (..),
  ReadyResponse (..),
  TxResponse (..),
  TxStatusResponse (..),
  TxStatusTimestamps (..),
  TxsResponse (..),
  UtxoResponse (..),
  UtxosResponse (..),
) where

import Data.Aeson (ToJSON, Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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

newtype TxResponse = TxResponse
  { tx :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype BlockResponse = BlockResponse
  { hashes :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data EncodedStoredUtxo = EncodedStoredUtxo
  { outref :: Text
  , outputCbor :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype UtxoResponse = UtxoResponse
  { utxo :: EncodedStoredUtxo
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype UtxosResponse = UtxosResponse
  { utxos :: [EncodedStoredUtxo]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype TxsResponse = TxsResponse
  { txs :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data TxStatusTimestamps = TxStatusTimestamps
  { createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data TxStatusResponse = TxStatusResponse
  { txId :: Text
  , status :: Text
  , reasonCode :: Maybe Text
  , reasonDetail :: Maybe Text
  , timestamps :: Maybe TxStatusTimestamps
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data DepositStatusResponse = DepositStatusResponse
  { eventId :: Text
  , eventInfo :: Text
  , inclusionTime :: UTCTime
  , cardanoTxHash :: Text
  , ledgerTxId :: Text
  , ledgerOutput :: Text
  , ledgerAddress :: Text
  , projectedHeaderHash :: Maybe Text
  , status :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data PlaceholderResponse = PlaceholderResponse
  { endpoint :: Text
  , status :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

data PlaceholderWithRequestResponse = PlaceholderWithRequestResponse
  { endpoint :: Text
  , status :: Text
  , request :: Value
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)
