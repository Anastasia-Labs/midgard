module Midgard.Node.Server.Tx.Types (
  AdditionalAssets (..),
  DepositBuildRequest (..),
  DepositBuildResponse (..),
  FundingUtxo (..),
  LucidAssetMap (..),
  SubmitAcceptedResponse (..),
  SubmitTxRequest (..),
) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype LucidAssetMap = LucidAssetMap
  { assets :: Map Text Text
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

newtype AdditionalAssets = AdditionalAssets
  { additionalAssets :: Map Text Value
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

data FundingUtxo = FundingUtxo
  { txHash :: Text
  , outputIndex :: Int
  , address :: Text
  , assets :: LucidAssetMap
  , datum :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data DepositBuildRequest = DepositBuildRequest
  { fundingAddress :: C.Address C.ShelleyAddr
  , fundingUtxos :: [FundingUtxo]
  , l2Address :: Text
  , l2Datum :: Maybe Text
  , lovelace :: Text
  , additionalAssets :: Maybe AdditionalAssets
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype DepositBuildResponse = DepositBuildResponse
  { unsignedTxCbor :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SubmitTxRequest = SubmitTxRequest
  { tx_cbor :: Maybe Text
  , txCbor :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SubmitAcceptedResponse = SubmitAcceptedResponse
  { txId :: Text
  , status :: Text
  , firstSeenAt :: UTCTime
  , lastSeenAt :: UTCTime
  , duplicate :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
