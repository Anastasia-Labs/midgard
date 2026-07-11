module Midgard.Node.Server.Protocol.Types (
  Network (..),
  ProtocolInfoApiVersion (..),
  ProtocolFeeParameters (..),
  ProtocolInfoResponse (..),
  ProtocolSubmissionLimits (..),
  ProtocolValidation (..),
  protocolInfoApiVersionToInt,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import Deriving.Aeson (
  CamelToSnake,
  ConstructorTagModifier,
  CustomJSON (CustomJSON),
  TagSingleConstructors,
 )
import Midgard.Node.Codec.Scripts (SupportedScriptLanguageTag)
import Midgard.Node.Server.JSON.Types (NaturalJSON)

data Network = Mainnet | Preview | Preprod | Custom
  deriving stock (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[TagSingleConstructors, ConstructorTagModifier '[CamelToSnake]] Network

data ProtocolInfoApiVersion = ProtocolInfoApiV_1
  deriving stock (Eq, Show)

protocolInfoApiVersionToInt :: ProtocolInfoApiVersion -> Int
protocolInfoApiVersionToInt ProtocolInfoApiV_1 = 1

instance ToJSON ProtocolInfoApiVersion where
  toJSON = toJSON . protocolInfoApiVersionToInt
  toEncoding = toEncoding . protocolInfoApiVersionToInt

instance FromJSON ProtocolInfoApiVersion where
  parseJSON value = do
    version <- parseJSON value :: Parser Int
    case version of
      1 -> pure ProtocolInfoApiV_1
      _ -> fail ("Unsupported protocol info API version: " <> show version)

data ProtocolFeeParameters = ProtocolFeeParameters
  { minFeeA :: NaturalJSON
  , minFeeB :: NaturalJSON
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype ProtocolSubmissionLimits = ProtocolSubmissionLimits
  { maxSubmitTxCborBytes :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ProtocolValidation = ProtocolValidation
  { strictnessProfile :: Text
  , localValidationIsAuthoritative :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ProtocolInfoResponse = ProtocolInfoResponse
  { apiVersion :: ProtocolInfoApiVersion
  , network :: Network
  , midgardNativeTxVersion :: Word
  , currentSlot :: NaturalJSON
  , supportedScriptLanguages :: [SupportedScriptLanguageTag]
  , protocolFeeParameters :: ProtocolFeeParameters
  , submissionLimits :: ProtocolSubmissionLimits
  , validation :: ProtocolValidation
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
