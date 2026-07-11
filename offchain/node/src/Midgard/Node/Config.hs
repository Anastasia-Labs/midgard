module Midgard.Node.Config (
  MidgardNodeConfig (..),
  ApiConfig (..),
  DbConnStr (..),
  DatabaseConfig (..),
  MidgardConfig (..),
  ProtocolConfig (..),
  ContractsConfig (..),
  FeatureFlags (..),
  loadConfigFile,
) where

import Data.Text (Text)

import Data.Aeson (FromJSON (..), withObject, withText, (.!=), (.:), (.:?))
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics (Generic)

newtype ApiConfig = ApiConfig
  { port :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | Helper bytestring wrapper with a text-based JSON instance.
newtype DbConnStr = DbConnStr {unDbConnStr :: ConnectionString}
  deriving stock (Eq, Show)

instance FromJSON DbConnStr where
  parseJSON = withText "DatabaseConnectionString" $ pure . DbConnStr . Text.encodeUtf8

data DatabaseConfig = DatabaseConfig
  { connectionString :: DbConnStr
  , poolSize :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data MidgardConfig = MidgardConfig
  { network :: !Text
  , adminApiKey :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ProtocolConfig = ProtocolConfig
  { minFeeA :: !Text
  , minFeeB :: !Text
  , maxSubmitTxCborBytes :: !Int
  , validationStrictnessProfile :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ProtocolConfig where
  parseJSON = withObject "ProtocolConfig" $ \obj ->
    ProtocolConfig
      <$> obj .:? "minFeeA" .!= "0"
      <*> obj .:? "minFeeB" .!= "0"
      <*> obj .:? "maxSubmitTxCborBytes" .!= 32768
      <*> obj .:? "validationStrictnessProfile" .!= "phase1_midgard"

data ContractsConfig = ContractsConfig
  { midgardEnvFile :: !(Maybe FilePath)
  , deploymentInfoFile :: !(Maybe FilePath)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype FeatureFlags = FeatureFlags
  { enableMutatingEndpoints :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data MidgardNodeConfig = MidgardNodeConfig
  { api :: !ApiConfig
  , database :: !DatabaseConfig
  , logLevel :: !Text
  , midgard :: !MidgardConfig
  , protocol :: !ProtocolConfig
  , contracts :: !ContractsConfig
  , features :: !FeatureFlags
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON MidgardNodeConfig where
  parseJSON = withObject "MidgardNodeConfig" $ \obj ->
    MidgardNodeConfig
      <$> obj .: "api"
      <*> obj .: "database"
      <*> obj .: "logLevel"
      <*> obj .: "midgard"
      <*> obj .:? "protocol" .!= ProtocolConfig "0" "0" 32768 "phase1_midgard"
      <*> obj .: "contracts"
      <*> obj .: "features"

loadConfigFile :: FilePath -> IO MidgardNodeConfig
loadConfigFile = Yaml.decodeFileThrow
