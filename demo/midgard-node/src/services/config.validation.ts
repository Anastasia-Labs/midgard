import { availableParallelism } from "node:os";

import { Config, Option } from "effect";

import { importCheckConfig } from "./config.import-check.js";
import {
  integerConfig,
  nonNegativeSafeIntegerConfig,
  positiveSafeIntegerConfig,
  stringConfig,
} from "./config.parse.js";

export const resolveValidationWorkerPoolSize = (
  configured: number | undefined,
  availableCpus = availableParallelism(),
): number => {
  if (configured === undefined) {
    return Math.max(1, availableCpus - 2);
  }
  if (!Number.isSafeInteger(configured) || configured < 0) {
    throw new Error(
      "VALIDATION_WORKER_POOL_SIZE must be a non-negative safe integer",
    );
  }
  return configured;
};

const feeCoefficient = (name: string) =>
  stringConfig(name, "0").pipe(Config.mapAttempt((value) => BigInt(value)));

/**
 * How the node validates admitted transactions: batching, the worker pool,
 * leases and retries, the minimum fee and the import checker.
 */
export const validationConfig = Config.all({
  VALIDATION_BATCH_SIZE: positiveSafeIntegerConfig(
    "VALIDATION_BATCH_SIZE",
    2_048,
  ),
  VALIDATION_BATCH_HARD_CAP: positiveSafeIntegerConfig(
    "VALIDATION_BATCH_HARD_CAP",
    8_192,
  ),
  VALIDATION_MIN_BATCH: positiveSafeIntegerConfig("VALIDATION_MIN_BATCH", 128),
  VALIDATION_PHASE_A_CONCURRENCY: positiveSafeIntegerConfig(
    "VALIDATION_PHASE_A_CONCURRENCY",
    32,
  ),
  VALIDATION_G4_BUCKET_CONCURRENCY: positiveSafeIntegerConfig(
    "VALIDATION_G4_BUCKET_CONCURRENCY",
    8,
  ),
  VALIDATION_STRICTNESS_PROFILE: stringConfig(
    "VALIDATION_STRICTNESS_PROFILE",
    "phase1_midgard",
  ),
  VALIDATION_WORKER_POOL_SIZE: Config.option(
    Config.integer("VALIDATION_WORKER_POOL_SIZE"),
  ).pipe(
    Config.mapAttempt((value) =>
      resolveValidationWorkerPoolSize(Option.getOrUndefined(value)),
    ),
  ),
  VALIDATION_WORKER_CHUNK_SIZE: positiveSafeIntegerConfig(
    "VALIDATION_WORKER_CHUNK_SIZE",
    64,
  ),
  VALIDATION_WORKER_INLINE_THRESHOLD: nonNegativeSafeIntegerConfig(
    "VALIDATION_WORKER_INLINE_THRESHOLD",
    32,
  ),
  VALIDATION_WORKER_JOB_TIMEOUT_MS: positiveSafeIntegerConfig(
    "VALIDATION_WORKER_JOB_TIMEOUT_MS",
    30_000,
  ),
  VALIDATION_DRAIN_LOOPS: positiveSafeIntegerConfig(
    "VALIDATION_DRAIN_LOOPS",
    4,
  ),
  VALIDATION_LEDGER_DELTA_LOG_MAX: positiveSafeIntegerConfig(
    "VALIDATION_LEDGER_DELTA_LOG_MAX",
    64,
  ),
  TX_QUEUE_POLL_INTERVAL_MS: positiveSafeIntegerConfig(
    "TX_QUEUE_POLL_INTERVAL_MS",
    250,
  ),
  VALIDATION_LEASE_MS: integerConfig("VALIDATION_LEASE_MS", 30_000),
  VALIDATION_RETRY_BACKOFF_BASE_MS: positiveSafeIntegerConfig(
    "VALIDATION_RETRY_BACKOFF_BASE_MS",
    250,
  ),
  VALIDATION_RETRY_BACKOFF_MAX_MS: positiveSafeIntegerConfig(
    "VALIDATION_RETRY_BACKOFF_MAX_MS",
    10_000,
  ),
  MIN_FEE_A: feeCoefficient("MIN_FEE_A"),
  MIN_FEE_B: feeCoefficient("MIN_FEE_B"),
  /** The import checker's kill and per-block cap (`config.import-check.ts`). */
  IMPORT_CHECK: importCheckConfig,
}).pipe(
  Config.mapAttempt((settings) => {
    if (settings.VALIDATION_MIN_BATCH > settings.VALIDATION_BATCH_HARD_CAP) {
      throw new Error(
        "VALIDATION_MIN_BATCH must not exceed VALIDATION_BATCH_HARD_CAP",
      );
    }
    if (
      settings.VALIDATION_WORKER_INLINE_THRESHOLD >
      settings.VALIDATION_BATCH_HARD_CAP
    ) {
      throw new Error(
        "VALIDATION_WORKER_INLINE_THRESHOLD must not exceed VALIDATION_BATCH_HARD_CAP",
      );
    }
    if (
      settings.VALIDATION_RETRY_BACKOFF_MAX_MS <
      settings.VALIDATION_RETRY_BACKOFF_BASE_MS
    ) {
      throw new Error(
        "VALIDATION_RETRY_BACKOFF_MAX_MS must not be less than VALIDATION_RETRY_BACKOFF_BASE_MS",
      );
    }
    return settings;
  }),
);
