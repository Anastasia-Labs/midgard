module Midgard.Node.Server.Health.Types (
  HealthResponseOk (..),
  HealthResponse (..),
  ReadinessResponse (..),
) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Deriving.Aeson (
  CamelToSnake,
  ConstructorTagModifier,
  CustomJSON (CustomJSON),
  FromJSON,
  StripPrefix,
  TagSingleConstructors,
  ToJSON,
 )
import GHC.Generics (Generic)

-- | Only valid success response to return at /healthz. Encodes into "ok".
data HealthResponseOk = HealthResponseOk
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[TagSingleConstructors, ConstructorTagModifier '[StripPrefix "HealthResponse", CamelToSnake]]
          HealthResponseOk

data HealthResponse = HealthResponse
  { status :: HealthResponseOk
  , now :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ReadinessResponse = ReadinessResponse
  { ready :: Bool
  , reasons :: [Text]
  , durableAdmissionBacklog :: Text
  , durableAdmissionOldestAgeMs :: Int
  , unfinishedLocalMutationJobs :: Text
  , unresolvedBlockSubmissionAgeMs :: Int
  , legacyInMemoryQueueDepth :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
