import * as SDK from "@al-ft/midgard-sdk";
import { Config, Option } from "effect";

import { validateRetentionDays } from "../database/retention-policy.js";
import {
  integerConfig,
  positiveSafeIntegerConfig,
  stringConfig,
} from "./config.parse.js";

/**
 * The node's Postgres database: the connection and its pools, the
 * write-behind queue, the mempool page size and housekeeping retention.
 */
export const databaseConfig = Config.all({
  POSTGRES_HOST: stringConfig("POSTGRES_HOST", "postgres"), // service name
  POSTGRES_PORT: integerConfig("POSTGRES_PORT", 5432),
  POSTGRES_PASSWORD: stringConfig("POSTGRES_PASSWORD", "postgres"),
  POSTGRES_DB: stringConfig("POSTGRES_DB", "midgard"),
  POSTGRES_USER: stringConfig("POSTGRES_USER", "postgres"),
  POSTGRES_ADMISSION_POOL_SIZE: positiveSafeIntegerConfig(
    "POSTGRES_ADMISSION_POOL_SIZE",
    10,
  ),
  POSTGRES_BATCH_POOL_SIZE: positiveSafeIntegerConfig(
    "POSTGRES_BATCH_POOL_SIZE",
    20,
  ),
  POSTGRES_WORKER_POOL_SIZE: positiveSafeIntegerConfig(
    "POSTGRES_WORKER_POOL_SIZE",
    10,
  ),
  MEMPOOL_RETRIEVE_PAGE_SIZE: positiveSafeIntegerConfig(
    "MEMPOOL_RETRIEVE_PAGE_SIZE",
    20_000,
  ),
  WRITE_BEHIND_FLUSH_INTERVAL_MS: positiveSafeIntegerConfig(
    "WRITE_BEHIND_FLUSH_INTERVAL_MS",
    100,
  ),
  WRITE_BEHIND_MAX_BATCH: positiveSafeIntegerConfig(
    "WRITE_BEHIND_MAX_BATCH",
    1_000,
  ),
  WRITE_BEHIND_QUEUE_CAPACITY: positiveSafeIntegerConfig(
    "WRITE_BEHIND_QUEUE_CAPACITY",
    50_000,
  ),
  /** Explicit housekeeping window in days; undefined when unset, which means
   * the verified deployment manifest's window
   * (`resolveHousekeepingRetentionDays`). A shorter explicit value refuses
   * startup in assertDeploymentManifestMatchesConfig (B5). DA payload
   * pruning ignores it. */
  RETENTION_DAYS: Config.option(Config.integer("RETENTION_DAYS")).pipe(
    Config.mapAttempt((value) =>
      Option.isNone(value) ? undefined : validateRetentionDays(value.value),
    ),
  ),
  WAIT_BETWEEN_RETENTION_SWEEPS: integerConfig(
    "WAIT_BETWEEN_RETENTION_SWEEPS",
    Math.min(900_000, Math.floor(Number(SDK.DA_ATTESTATION_TIMEOUT_MS) / 4)),
  ),
});
