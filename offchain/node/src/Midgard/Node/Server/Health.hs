module Midgard.Node.Server.Health (
  healthServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Midgard.Node.Migrations qualified as Migrations
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
  migrationFiles <- liftIO Migrations.listSqlMigrations
  -- The DB-backed checks are parked with the rest of the DB layer. For now,
  -- readiness only verifies that we can still discover the shared SQL files
  -- under ../demo/midgard-node; worker and schema checks come back with DB.
  let schemaReady = not (null migrationFiles)
  let reasons =
        ["sql_migrations_missing" | null migrationFiles]
  pure
    ReadinessResponse
      { ready = schemaReady
      , reasons
      , -- TODO(indexer): replace these queue placeholders with real durable
        -- admission/indexer worker metrics once the node worker sublibrary lands.
        durableAdmissionBacklog = "0"
      , durableAdmissionOldestAgeMs = 0
      , unfinishedLocalMutationJobs = "0"
      , unresolvedBlockSubmissionAgeMs = 0
      , legacyInMemoryQueueDepth = 0
      }
