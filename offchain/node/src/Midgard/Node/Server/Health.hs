module Midgard.Node.Server.Health (
  healthServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Midgard.Node.Server.Api (HealthAPI)
import Midgard.Node.Server.Health.Types (HealthResponse (..), HealthResponseOk (..), ReadinessResponse (..))
import Midgard.Node.Server.Monad (ServerM)
import Servant (ServerT, type (:<|>) (..))

healthServer :: ServerT HealthAPI ServerM
healthServer =
  healthHandler
    :<|> readinessHandler

healthHandler :: ServerM HealthResponse
healthHandler = do
  now <- liftIO getCurrentTime
  pure $ HealthResponse {status = HealthResponseOk, now}

readinessHandler :: ServerM ReadinessResponse
readinessHandler = do
  -- TODO: Add readiness checks.
  pure
    ReadinessResponse
      { ready = True
      , reasons = []
      , -- TODO(indexer): replace these queue placeholders with real durable
        -- admission/indexer worker metrics once the node worker sublibrary lands.
        durableAdmissionBacklog = "0"
      , durableAdmissionOldestAgeMs = 0
      , unfinishedLocalMutationJobs = "0"
      , unresolvedBlockSubmissionAgeMs = 0
      , legacyInMemoryQueueDepth = 0
      }
