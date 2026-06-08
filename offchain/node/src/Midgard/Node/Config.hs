module Midgard.Node.Config (
  MidgardNodeConfig (..),
  ApiConfig (..),
  DatabaseConfig (..),
  MidgardConfig (..),
  ContractsConfig (..),
  FeatureFlags (..),
  loadConfigFile,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Yaml qualified as Yaml

data ApiConfig = ApiConfig
  { port :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON ApiConfig where
  parseJSON = withObject "ApiConfig" $ \obj ->
    ApiConfig <$> obj .: "port"

data DatabaseConfig = DatabaseConfig
  { connectionString :: Text
  , poolSize :: Maybe Int
  }
  deriving stock (Eq, Show)

instance FromJSON DatabaseConfig where
  parseJSON = withObject "DatabaseConfig" $ \obj ->
    DatabaseConfig
      <$> obj .: "connectionString"
      <*> obj .:? "poolSize"

data MidgardConfig = MidgardConfig
  { network :: Text
  , adminApiKey :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromJSON MidgardConfig where
  parseJSON = withObject "MidgardConfig" $ \obj ->
    MidgardConfig
      <$> obj .: "network"
      <*> obj .:? "adminApiKey"

data ContractsConfig = ContractsConfig
  { midgardEnvFile :: Maybe FilePath
  , deploymentInfoFile :: Maybe FilePath
  }
  deriving stock (Eq, Show)

instance FromJSON ContractsConfig where
  parseJSON = withObject "ContractsConfig" $ \obj ->
    ContractsConfig
      <$> obj .:? "midgardEnvFile"
      <*> obj .:? "deploymentInfoFile"

data FeatureFlags = FeatureFlags
  { enableMutatingEndpoints :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON FeatureFlags where
  parseJSON = withObject "FeatureFlags" $ \obj ->
    FeatureFlags
      <$> obj .: "enableMutatingEndpoints"

data MidgardNodeConfig = MidgardNodeConfig
  { api :: ApiConfig
  , database :: Maybe DatabaseConfig
  , logLevel :: Text
  , midgard :: MidgardConfig
  , contracts :: ContractsConfig
  , features :: FeatureFlags
  }
  deriving stock (Eq, Show)

instance FromJSON MidgardNodeConfig where
  parseJSON = withObject "MidgardNodeConfig" $ \obj ->
    MidgardNodeConfig
      <$> obj .: "api"
      <*> obj .:? "database"
      <*> obj .: "logLevel"
      <*> obj .: "midgard"
      <*> obj .: "contracts"
      <*> obj .: "features"

loadConfigFile :: FilePath -> IO MidgardNodeConfig
loadConfigFile = Yaml.decodeFileThrow
