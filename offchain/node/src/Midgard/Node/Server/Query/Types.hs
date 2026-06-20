module Midgard.Node.Server.Query.Types (
  BlockResponse (..),
  DepositStatusResponse (..),
  EncodedStoredUtxo (..),
  TxResponse (..),
  TxStatusResponse (..),
  TxStatusTimestamps (..),
  TxsResponse (..),
  UtxoResponse (..),
  UtxosResponse (..),
) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Deriving.Aeson (
  CamelToSnake,
  ConstructorTagModifier,
  CustomJSON (CustomJSON),
  StripPrefix,
  TagSingleConstructors,
 )
import Midgard.Node.Server.JSON.Types (TxJSON, TxOutJSON, TxOutRefJSON)

newtype TxResponse = TxResponse
  { tx :: TxJSON
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype BlockResponse = BlockResponse
  { hashes :: [C.TxId]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data EncodedStoredUtxo = EncodedStoredUtxo
  { outref :: TxOutRefJSON
  , outputCbor :: TxOutJSON
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
  { txs :: [TxJSON]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

newtype TxStatusTimestamps = TxStatusTimestamps
  { createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data TxStatus
  = TxStatusCommitted
  | TxStatusAccepted
  | TxStatusPendingCommit
  | TxStatusAwaitingLocalRecovery
  | TxStatusValidating
  | TxStatusQueued
  | TxStatusNotFound
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[TagSingleConstructors, ConstructorTagModifier '[StripPrefix "TxStatus", CamelToSnake]]
          TxStatus

data TxStatusResponse
  = TxStatusSimpleResponse
      { txId :: C.TxId
      , status :: TxStatus
      }
  | TxStatusRejectedResponse
      { txId :: C.TxId
      , status :: TxStatus
      , reasonCode :: Text
      , reasonDetail :: Maybe Text
      , timestamps :: TxStatusTimestamps
      }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

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
  deriving anyclass (FromJSON, ToJSON)
