import { availableParallelism } from "node:os";

import {
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  type DeploymentManifestEconomicsProfile,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
} from "@al-ft/midgard-sdk";
import { Network, UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Config, Context, Data, Effect, Layer, Option } from "effect";

import {
  positiveFiniteNumber,
  positiveSafeInteger,
} from "../artifact-schema.js";
import { readDaHardeningConfig } from "../da/hardening-config.js";
import {
  isStrictlyAscending,
  splitPackedHex,
  VERIFICATION_KEY_HASH_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "../da/local-signers.js";
import {
  assertRetentionDaysMatchesDeployment,
  validateRetentionDays,
} from "../database/retention-policy.js";
import { parseDeploymentEconomicsProfile } from "../environment.js";

/**
 * Validates the *encoding* of one of the DA key sets (`DA_COMMITTEE_HEX`,
 * `DA_OWNERS_HEX`) at config load, returning the normalized packed hex.
 *
 * Encoding only — deliberately not policy. `DA_COMMITTEE_HEX`, `DA_OWNERS_HEX`
 * and `DA_THRESHOLD` are read by exactly one consumer,
 * `deriveOperatorDaParams`, and every other subsystem that loads `NodeConfig`
 * ignores them. Enforcing the Q63 governed floors here would let a stale
 * deployment value (say the pre-Q63 `DA_THRESHOLD=1` still sitting in a
 * checkout's `.env`) fail config load for the whole process, surfacing as an
 * opaque error inside subsystems that never touch DA. The floors are instead
 * enforced in `deriveOperatorDaParams`, at the one point where a
 * governor-invalid datum would actually be written, where the real committee
 * length is known even when it is derived from local signers rather than
 * configured.
 *
 * What stays here is what is unambiguously wrong regardless of policy: a value
 * that is not hex, is not a whole number of elements, or is not the
 * sorted-unique ascending order the governor's walkers require.
 */
const validateDaKeySetEncoding = (
  value: string,
  chunkHexLength: number,
  fieldName: string,
  shape: string,
): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length === 0) {
    return "";
  }
  let elements: readonly string[];
  try {
    elements = splitPackedHex(normalized, chunkHexLength, fieldName);
  } catch {
    throw new Error(`${fieldName} must be ${shape} as hex`);
  }
  if (!isStrictlyAscending(elements)) {
    throw new Error(
      `${fieldName} must be sorted ascending with no duplicates, matching the governor's sorted-unique encoding`,
    );
  }
  return normalized;
};

/**
 * Configuration loading for the Midgard node process.
 *
 * This module centralizes environment-variable decoding, defaulting, and the
 * derived values that other services depend on. Keeping it in one place makes
 * production configuration easier to audit.
 */
type Provider = "Kupmios";

/**
 * The SQL quota counts each unique material entry (32-byte root plus at most
 * six bytes of DA-value framing), its membership row (32 + 32 + boolean), and
 * its admission-owner row (32 + 32 + 32). Reserving 199 bytes per maximum
 * reachable node in addition to the authenticated preimages guarantees that
 * one protocol-valid maximum envelope fits even at the configured minimum.
 */
export const CEK_PROGRAM_MATERIAL_MIN_STORE_BYTES = Number(
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES +
    MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT * 199n,
);

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

const boundedValidationInteger = (
  name: string,
  defaultValue: number,
  allowZero = false,
) =>
  Config.integer(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => {
      if (
        !Number.isSafeInteger(value) ||
        (allowZero ? value < 0 : value <= 0)
      ) {
        throw new Error(
          `${name} must be a ${allowZero ? "non-negative" : "positive"} safe integer`,
        );
      }
      return value;
    }),
  );

/**
 * Fully-decoded runtime configuration required by the node.
 */
export type NodeConfigDep = {
  L1_PROVIDER: Provider;
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: number;
  L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: number;
  L1_RECENT_TX_VISIBILITY_TIMEOUT_MS: number;
  L1_RECENT_TX_404_MAX_DELAY_MS: number;
  L1_OGMIOS_KEY: string;
  L1_KUPO_KEY: string;
  L1_OPERATOR_SEED_PHRASE: string;
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  L1_REFERENCE_SCRIPT_SEED_PHRASE: string;
  L1_REFERENCE_SCRIPT_ADDRESS: string;
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: string;
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS: number;
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS: number;
  NETWORK: Network;
  MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: DeploymentManifestEconomicsProfile;
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENT: number;
  WAIT_BETWEEN_BLOCK_CONFIRMATION: number;
  SPECULATIVE_COMMIT_BUILD: boolean;
  SPECULATIVE_REBUILD_MAX_ATTEMPTS: number;
  USER_EVENT_BARRIER_REFRESH_MS: number;
  USER_EVENT_BARRIER_MAX_STALENESS_MS: number;
  USER_EVENT_INCLUSION_DEADLINE_MS: number;
  BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: number;
  BLOCK_CONFIRMATION_AWAIT_RETRIES: number;
  UNCONFIRMED_BLOCK_MAX_AGE_MS: number;
  WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  MIN_QUEUE_LENGTH_FOR_MERGING: number;
  VALIDATION_BATCH_SIZE: number;
  VALIDATION_BATCH_HARD_CAP: number;
  VALIDATION_MIN_BATCH: number;
  VALIDATION_MAX_QUEUE_AGE_MS: number;
  VALIDATION_PHASE_A_CONCURRENCY: number;
  VALIDATION_G4_BUCKET_CONCURRENCY: number;
  VALIDATION_STRICTNESS_PROFILE: string;
  VALIDATION_WORKER_POOL_SIZE: number;
  VALIDATION_WORKER_CHUNK_SIZE: number;
  VALIDATION_WORKER_INLINE_THRESHOLD: number;
  VALIDATION_WORKER_JOB_TIMEOUT_MS: number;
  VALIDATION_WORKER_NODE_ED25519: boolean;
  VALIDATION_DRAIN_LOOPS: number;
  VALIDATION_UPLC_IN_WORKERS: boolean;
  VALIDATION_LEDGER_DELTA_LOG_MAX: number;
  TX_QUEUE_POLL_INTERVAL_MS: number;
  MIN_FEE_A: bigint;
  MIN_FEE_B: bigint;
  RUN_GENESIS_ON_STARTUP: boolean;
  ADMIN_API_KEY: string;
  MAX_DURABLE_ADMISSION_BACKLOG: number;
  MAX_DURABLE_ADMISSION_BACKLOG_BYTES: number;
  SUBMIT_INGRESS_MAX_CONCURRENCY: number;
  SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES: number;
  CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: number;
  MAX_SUBMIT_TX_CBOR_BYTES: number;
  READINESS_MAX_HEARTBEAT_AGE_MS: number;
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: number;
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: number;
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: number;
  STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS: number;
  STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: number;
  VALIDATION_LEASE_MS: number;
  VALIDATION_RETRY_BACKOFF_BASE_MS: number;
  VALIDATION_RETRY_BACKOFF_MAX_MS: number;
  VALIDATION_EXPIRED_LEASE_READINESS_THRESHOLD: number;
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: number;
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: number;
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: number;
  STATE_QUEUE_CORRECTION_FINALITY_DEPTH: number;
  RETENTION_DAYS: number;
  WAIT_BETWEEN_RETENTION_SWEEPS: number;
  HUB_ORACLE_ONE_SHOT_TX_HASH: string;
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  OPERATOR_REQUIRED_BOND_LOVELACE: bigint;
  OPERATOR_SLASHING_PENALTY_LOVELACE: bigint;
  DA_COMMITTEE_HEX: string;
  DA_THRESHOLD: bigint | null;
  DA_OWNERS_HEX: string;
  DA_COSIGNER_SEED_PHRASE: string;
  MIDGARD_DA_PAYLOAD_ENVELOPE: "identity" | "zstd";
  MIDGARD_DA_ZSTD_LEVEL: number;
  MIDGARD_DA_PUBLISH_CONCURRENCY: number;
  MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: number;
  MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS: number;
  MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS: number;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  POSTGRES_PORT: number;
  POSTGRES_ADMISSION_POOL_SIZE: number;
  POSTGRES_BATCH_POOL_SIZE: number;
  POSTGRES_WORKER_POOL_SIZE: number;
  ADMISSION_BACKLOG_REFRESH_MS: number;
  MEMPOOL_RETRIEVE_PAGE_SIZE: number;
  WRITE_BEHIND_FLUSH_INTERVAL_MS: number;
  WRITE_BEHIND_MAX_BATCH: number;
  WRITE_BEHIND_QUEUE_CAPACITY: number;
  MPF_ENGINE: "legacy" | "overlay" | "event_flat" | "architecture_g";
  MPF_SCRATCH_BUILD: "insert" | "fromlist";
  MPF_PATH_HYDRATION_MODE: "whole_block" | "chunked" | "chunked_arena";
  MPF_HYDRATION_CHUNK_OPS: number;
  MPF_RETAIN_HYDRATED_DEPTH: number;
  MPF_OVERLAY_SPILL_BYTES: number;
  MPF_PAYLOAD_ROOT_CHECK: "every_block" | "periodic" | "off";
  MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS: number;
  MPF_PAYLOAD_AUDIT_INTERVAL_MS: number;
  MPF_PARALLEL_ROOTS: boolean;
  MPF_ROOT_WORKERS: number;
  MPF_PARALLEL_ROOT_MIN_ENTRIES: number;
  COMMIT_MAX_L2_TX_COUNT: number;
  COMMIT_MAX_LEDGER_OP_COUNT: number;
  COMMIT_MAX_TRANSITION_STEP_COUNT: number;
  COMMIT_BUILD_COST_MODEL: "static" | "ewma";
  COMMIT_BUILD_EWMA_ALPHA: number;
  COMMIT_BUILD_EWMA_SAFETY_FACTOR: number;
  MPF_RECORD_CORPUS: string;
  MPF_NATIVE_OWNER_BINARY_PATH: string;
  MPF_NATIVE_OWNER_BINARY_SHA256: string;
  MPF_NATIVE_OWNER_SIDECAR_PATH: string;
  MPF_NATIVE_OWNER_MAX_FRAME_BYTES: number;
  MPF_NATIVE_OWNER_MAX_CHUNK_BYTES: number;
  MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS: number;
  MPF_NATIVE_OWNER_RESTART_LIMIT: number;
  LEDGER_MPF_DB_PATH: string;
  TRANSACTIONS_MPF_DB_PATH: string;
  GENESIS_UTXOS: UTxO[];
  /** Preserves configured wallet identity when an isolated harness maps C=A. */
  GENESIS_UTXOS_BY_WALLET?: Readonly<{
    A: readonly UTxO[];
    B: readonly UTxO[];
    C: readonly UTxO[];
  }>;
};

const positiveSafeIntegerConfig = (name: string, defaultValue: number) =>
  Config.integer(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => positiveSafeInteger(value, name)),
  );

const positiveFiniteNumberConfig = (name: string, defaultValue: number) =>
  Config.number(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => positiveFiniteNumber(value, name)),
  );

/**
 * Loads and normalizes the node's runtime configuration from environment
 * variables.
 */
const makeConfig = Effect.gen(function* () {
  const provider = yield* Config.literal("Kupmios")("L1_PROVIDER");
  const ogmiosKey = yield* Config.string("L1_OGMIOS_KEY");
  const kupoKey = yield* Config.string("L1_KUPO_KEY");
  const operatorSeedPhrase = yield* Config.string("L1_OPERATOR_SEED_PHRASE");
  const operatorSeedPhraseForMergeTx = yield* Config.string(
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  );
  const network = yield* Config.literal(
    "Mainnet",
    "Preprod",
    "Preview",
    "Custom",
  )("NETWORK");
  const deploymentEconomicsProfile = yield* Config.string(
    "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE",
  ).pipe(Config.mapAttempt(parseDeploymentEconomicsProfile));
  const deploymentEconomics =
    DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[deploymentEconomicsProfile];
  const l1ProviderPreflightTimeoutMs = yield* Config.integer(
    "L1_PROVIDER_PREFLIGHT_TIMEOUT_MS",
  ).pipe(
    Config.withDefault(15_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "L1_PROVIDER_PREFLIGHT_TIMEOUT_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const l1ProviderRateLimitCooldownMs = yield* Config.integer(
    "L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS",
  ).pipe(
    Config.withDefault(60_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const l1RecentTxVisibilityTimeoutMs = yield* Config.integer(
    "L1_RECENT_TX_VISIBILITY_TIMEOUT_MS",
  ).pipe(
    Config.withDefault(180_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "L1_RECENT_TX_VISIBILITY_TIMEOUT_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const l1RecentTx404MaxDelayMs = yield* Config.integer(
    "L1_RECENT_TX_404_MAX_DELAY_MS",
  ).pipe(
    Config.withDefault(10_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "L1_RECENT_TX_404_MAX_DELAY_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const referenceScriptSeedPhrase = yield* Config.string(
    "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  ).pipe(Config.withDefault(operatorSeedPhrase));
  const configuredReferenceScriptAddress = yield* Config.string(
    "L1_REFERENCE_SCRIPT_ADDRESS",
  ).pipe(Config.withDefault(""));
  const referenceScriptAuthTimelockMs = yield* Config.integer(
    "REFERENCE_SCRIPT_AUTH_TIMELOCK_MS",
  ).pipe(
    Config.withDefault(REFERENCE_SCRIPT_AUTH_TIMELOCK_MS),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "REFERENCE_SCRIPT_AUTH_TIMELOCK_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const referenceScriptAuthMinRemainingMs = yield* Config.integer(
    "REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS",
  ).pipe(
    Config.withDefault(REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS must be a positive safe integer",
        );
      }
      if (value >= referenceScriptAuthTimelockMs) {
        throw new Error(
          "REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS must be lower than REFERENCE_SCRIPT_AUTH_TIMELOCK_MS",
        );
      }
      return value;
    }),
  );
  const derivedReferenceScriptAddress = walletFromSeed(
    referenceScriptSeedPhrase,
    {
      network,
    },
  ).address;
  const referenceScriptAddress =
    configuredReferenceScriptAddress.trim() || derivedReferenceScriptAddress;
  const referenceScriptDeployAddress = yield* Config.string(
    "L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS",
  ).pipe(Config.withDefault(referenceScriptAddress));
  const port = yield* Config.integer("PORT").pipe(Config.withDefault(3000));
  const waitBetweenBlockCommitment = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_COMMITMENT",
  ).pipe(Config.withDefault(1000));
  const waitBetweenBlockConfirmation = yield* Config.integer(
    "WAIT_BETWEEN_BLOCK_CONFIRMATION",
  ).pipe(Config.withDefault(2000));
  const speculativeCommitBuild = yield* Config.boolean(
    "SPECULATIVE_COMMIT_BUILD",
  ).pipe(Config.withDefault(false));
  const speculativeRebuildMaxAttempts = yield* boundedValidationInteger(
    "SPECULATIVE_REBUILD_MAX_ATTEMPTS",
    3,
  );
  const userEventBarrierRefreshMs = yield* boundedValidationInteger(
    "USER_EVENT_BARRIER_REFRESH_MS",
    2_000,
  );
  const userEventBarrierMaxStalenessMs = yield* boundedValidationInteger(
    "USER_EVENT_BARRIER_MAX_STALENESS_MS",
    15_000,
  );
  const userEventInclusionDeadlineMs = yield* boundedValidationInteger(
    "USER_EVENT_INCLUSION_DEADLINE_MS",
    60_000,
  );
  if (
    userEventBarrierRefreshMs + userEventBarrierMaxStalenessMs >=
    userEventInclusionDeadlineMs
  ) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "User-event barrier freshness window exceeds the configured inclusion deadline",
        cause: `${userEventBarrierRefreshMs.toString()} + ${userEventBarrierMaxStalenessMs.toString()} >= ${userEventInclusionDeadlineMs.toString()}`,
        fieldsAndValues: [
          [
            "USER_EVENT_BARRIER_REFRESH_MS",
            userEventBarrierRefreshMs.toString(),
          ],
          [
            "USER_EVENT_BARRIER_MAX_STALENESS_MS",
            userEventBarrierMaxStalenessMs.toString(),
          ],
          [
            "USER_EVENT_INCLUSION_DEADLINE_MS",
            userEventInclusionDeadlineMs.toString(),
          ],
        ],
      }),
    );
  }
  const blockConfirmationAwaitTimeoutMs = yield* Config.integer(
    "BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS",
  ).pipe(Config.withDefault(12_000));
  const blockConfirmationAwaitRetries = yield* Config.integer(
    "BLOCK_CONFIRMATION_AWAIT_RETRIES",
  ).pipe(Config.withDefault(1));
  const unconfirmedBlockMaxAgeMs = yield* Config.integer(
    "UNCONFIRMED_BLOCK_MAX_AGE_MS",
  ).pipe(Config.withDefault(180_000));
  const waitBetweenMergeTxs = yield* Config.integer(
    "WAIT_BETWEEN_MERGE_TXS",
  ).pipe(Config.withDefault(10000));
  const minQueueLengthForMerging = yield* Config.integer(
    "MIN_QUEUE_LENGTH_FOR_MERGING",
  ).pipe(Config.withDefault(8));
  const validationBatchSize = yield* boundedValidationInteger(
    "VALIDATION_BATCH_SIZE",
    2_048,
  );
  const validationBatchHardCap = yield* boundedValidationInteger(
    "VALIDATION_BATCH_HARD_CAP",
    8_192,
  );
  const validationMinBatch = yield* boundedValidationInteger(
    "VALIDATION_MIN_BATCH",
    128,
  );
  const validationMaxQueueAgeMs = yield* Config.integer(
    "VALIDATION_MAX_QUEUE_AGE_MS",
  ).pipe(Config.withDefault(250));
  const validationPhaseAConcurrency = yield* boundedValidationInteger(
    "VALIDATION_PHASE_A_CONCURRENCY",
    32,
  );
  const validationG4BucketConcurrency = yield* boundedValidationInteger(
    "VALIDATION_G4_BUCKET_CONCURRENCY",
    8,
  );
  const validationStrictnessProfile = yield* Config.string(
    "VALIDATION_STRICTNESS_PROFILE",
  ).pipe(Config.withDefault("phase1_midgard"));
  const configuredValidationWorkerPoolSize = yield* Config.option(
    Config.integer("VALIDATION_WORKER_POOL_SIZE"),
  );
  const validationWorkerPoolSize = resolveValidationWorkerPoolSize(
    Option.getOrUndefined(configuredValidationWorkerPoolSize),
  );
  const validationWorkerChunkSize = yield* boundedValidationInteger(
    "VALIDATION_WORKER_CHUNK_SIZE",
    64,
  );
  const validationWorkerInlineThreshold = yield* boundedValidationInteger(
    "VALIDATION_WORKER_INLINE_THRESHOLD",
    32,
    true,
  );
  const validationWorkerJobTimeoutMs = yield* boundedValidationInteger(
    "VALIDATION_WORKER_JOB_TIMEOUT_MS",
    30_000,
  );
  const validationWorkerNodeEd25519 = yield* Config.boolean(
    "VALIDATION_WORKER_NODE_ED25519",
  ).pipe(Config.withDefault(true));
  const validationDrainLoops = yield* boundedValidationInteger(
    "VALIDATION_DRAIN_LOOPS",
    4,
  );
  const validationUplcInWorkers = yield* Config.boolean(
    "VALIDATION_UPLC_IN_WORKERS",
  ).pipe(Config.withDefault(true));
  const validationLedgerDeltaLogMax = yield* boundedValidationInteger(
    "VALIDATION_LEDGER_DELTA_LOG_MAX",
    64,
  );
  const txQueuePollIntervalMs = yield* boundedValidationInteger(
    "TX_QUEUE_POLL_INTERVAL_MS",
    250,
  );
  if (validationMinBatch > validationBatchHardCap) {
    throw new Error(
      "VALIDATION_MIN_BATCH must not exceed VALIDATION_BATCH_HARD_CAP",
    );
  }
  if (validationWorkerInlineThreshold > validationBatchHardCap) {
    throw new Error(
      "VALIDATION_WORKER_INLINE_THRESHOLD must not exceed VALIDATION_BATCH_HARD_CAP",
    );
  }
  const minFeeA = yield* Config.string("MIN_FEE_A").pipe(
    Config.withDefault("0"),
    Config.mapAttempt((value) => BigInt(value)),
  );
  const minFeeB = yield* Config.string("MIN_FEE_B").pipe(
    Config.withDefault("0"),
    Config.mapAttempt((value) => BigInt(value)),
  );
  const runGenesisOnStartup = yield* Config.string(
    "RUN_GENESIS_ON_STARTUP",
  ).pipe(
    Config.withDefault("false"),
    Config.map((value) => value.trim().toLowerCase() === "true"),
  );
  const adminApiKey = yield* Config.string("ADMIN_API_KEY").pipe(
    Config.withDefault(""),
  );
  const maxDurableAdmissionBacklog = yield* Config.integer(
    "MAX_DURABLE_ADMISSION_BACKLOG",
  ).pipe(Config.withDefault(10_000));
  const maxDurableAdmissionBacklogBytes = yield* Config.integer(
    "MAX_DURABLE_ADMISSION_BACKLOG_BYTES",
  ).pipe(
    Config.withDefault(MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "MAX_DURABLE_ADMISSION_BACKLOG_BYTES must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const submitIngressMaxConcurrency = yield* positiveSafeIntegerConfig(
    "SUBMIT_INGRESS_MAX_CONCURRENCY",
    4,
  );
  const submitIngressMaxInFlightBytes = yield* Config.integer(
    "SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES",
  ).pipe(
    Config.withDefault(MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes),
    Config.mapAttempt((value) => {
      if (
        !Number.isSafeInteger(value) ||
        value < MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes
      ) {
        throw new Error(
          `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES must be a safe integer at least ${MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes.toString()}`,
        );
      }
      return value;
    }),
  );
  const cekProgramMaterialStoreMaxBytes = yield* Config.integer(
    "CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES",
  ).pipe(
    Config.withDefault(CEK_PROGRAM_MATERIAL_MIN_STORE_BYTES * 4),
    Config.mapAttempt((value) => {
      if (
        !Number.isSafeInteger(value) ||
        value < CEK_PROGRAM_MATERIAL_MIN_STORE_BYTES
      ) {
        throw new Error(
          `CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES must be a safe integer at least ${CEK_PROGRAM_MATERIAL_MIN_STORE_BYTES.toString()}`,
        );
      }
      return value;
    }),
  );
  const maxSubmitTxCborBytes = yield* Config.integer(
    "MAX_SUBMIT_TX_CBOR_BYTES",
  ).pipe(
    Config.withDefault(MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes),
    Config.mapAttempt((value) => {
      if (
        !Number.isSafeInteger(value) ||
        value <= 0 ||
        value > MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes
      ) {
        throw new Error(
          `MAX_SUBMIT_TX_CBOR_BYTES must be between 1 and ${MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes.toString()}`,
        );
      }
      return value;
    }),
  );
  const readinessMaxHeartbeatAgeMs = yield* Config.integer(
    "READINESS_MAX_HEARTBEAT_AGE_MS",
  ).pipe(Config.withDefault(120_000));
  const readinessL1ProviderEvidenceMaxAgeMs = yield* positiveSafeIntegerConfig(
    "READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS",
    30_000,
  );
  const readinessMaxDurableAdmissionBacklog = yield* Config.integer(
    "READINESS_MAX_DURABLE_ADMISSION_BACKLOG",
  ).pipe(Config.withDefault(10_000));
  const readinessMaxDurableAdmissionAgeMs = yield* Config.integer(
    "READINESS_MAX_DURABLE_ADMISSION_AGE_MS",
  ).pipe(Config.withDefault(120_000));
  const startupProtocolStatusQueryMaxAttempts = yield* Config.integer(
    "STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS",
  ).pipe(Config.withDefault(120));
  const startupProtocolStatusQueryRetryDelayMs = yield* Config.integer(
    "STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS",
  ).pipe(Config.withDefault(5_000));
  const validationLeaseMs = yield* Config.integer("VALIDATION_LEASE_MS").pipe(
    Config.withDefault(30_000),
  );
  const validationRetryBackoffBaseMs = yield* positiveSafeIntegerConfig(
    "VALIDATION_RETRY_BACKOFF_BASE_MS",
    250,
  );
  const validationRetryBackoffMaxMs = yield* positiveSafeIntegerConfig(
    "VALIDATION_RETRY_BACKOFF_MAX_MS",
    10_000,
  );
  if (validationRetryBackoffMaxMs < validationRetryBackoffBaseMs) {
    throw new Error(
      "VALIDATION_RETRY_BACKOFF_MAX_MS must not be less than VALIDATION_RETRY_BACKOFF_BASE_MS",
    );
  }
  const validationExpiredLeaseReadinessThreshold = yield* Config.integer(
    "VALIDATION_EXPIRED_LEASE_READINESS_THRESHOLD",
  ).pipe(Config.withDefault(1));
  const stateQueueMutationLeaseTtlMs = yield* Config.integer(
    "STATE_QUEUE_MUTATION_LEASE_TTL_MS",
  ).pipe(
    Config.withDefault(10 * 60 * 1000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "STATE_QUEUE_MUTATION_LEASE_TTL_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const stateQueueMutationLeaseRenewIntervalMs = yield* Config.integer(
    "STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS",
  ).pipe(
    Config.withDefault(
      Math.min(
        60 * 1000,
        Math.max(1, Math.floor(stateQueueMutationLeaseTtlMs / 3)),
      ),
    ),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS must be a positive safe integer",
        );
      }
      if (value >= stateQueueMutationLeaseTtlMs) {
        throw new Error(
          "STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS must be less than STATE_QUEUE_MUTATION_LEASE_TTL_MS",
        );
      }
      return value;
    }),
  );
  const stateQueueMutationLeaseStaleGraceMs = yield* Config.integer(
    "STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS",
  ).pipe(
    Config.withDefault(60 * 1000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0) {
        throw new Error(
          "STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS must be a non-negative safe integer",
        );
      }
      return value;
    }),
  );
  const stateQueueCorrectionFinalityDepth = yield* Config.integer(
    "STATE_QUEUE_CORRECTION_FINALITY_DEPTH",
  ).pipe(
    Config.withDefault(30),
    Config.mapAttempt((value) => {
      if (value !== 30) {
        throw new Error(
          "STATE_QUEUE_CORRECTION_FINALITY_DEPTH must equal the F04 public/testnet release depth of 30 blocks",
        );
      }
      return value;
    }),
  );
  const retentionDays = yield* Config.integer("RETENTION_DAYS").pipe(
    Config.withDefault(0),
    Config.mapAttempt(validateRetentionDays),
    // Q54: enabled pruning must cover the deployment manifest's
    // da.transportProfile.retentionDays window; a shorter env value fails the
    // config load rather than silently pruning challengeable evidence.
    Config.mapAttempt((value) => assertRetentionDaysMatchesDeployment(value)),
  );
  const waitBetweenRetentionSweeps = yield* Config.integer(
    "WAIT_BETWEEN_RETENTION_SWEEPS",
  ).pipe(Config.withDefault(3_600_000));
  const hubOracleOneShotTxHash = yield* Config.string(
    "HUB_ORACLE_ONE_SHOT_TX_HASH",
  ).pipe(Config.withDefault(""));
  const hubOracleOneShotOutputIndex = yield* Config.integer(
    "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX",
  ).pipe(Config.withDefault(-1));
  const operatorRequiredBondLovelace = yield* Config.string(
    "OPERATOR_REQUIRED_BOND_LOVELACE",
  ).pipe(
    Config.withDefault(deploymentEconomics.requiredBondLovelace.toString()),
    Config.mapAttempt((value) => {
      const parsed = BigInt(value);
      const expected = BigInt(deploymentEconomics.requiredBondLovelace);
      if (parsed !== expected) {
        throw new Error(
          `OPERATOR_REQUIRED_BOND_LOVELACE must equal ${deploymentEconomicsProfile} profile economics ${expected.toString()}`,
        );
      }
      return parsed;
    }),
  );
  const operatorSlashingPenaltyLovelace = yield* Config.string(
    "OPERATOR_SLASHING_PENALTY_LOVELACE",
  ).pipe(
    Config.withDefault(deploymentEconomics.slashingPenaltyLovelace.toString()),
    Config.mapAttempt((value) => {
      const parsed = BigInt(value);
      const expected = BigInt(deploymentEconomics.slashingPenaltyLovelace);
      if (parsed !== expected) {
        throw new Error(
          `OPERATOR_SLASHING_PENALTY_LOVELACE must equal ${deploymentEconomicsProfile} profile economics ${expected.toString()}`,
        );
      }
      return parsed;
    }),
  );
  // Q63 (F04 §4) governed floors are enforced in `deriveOperatorDaParams`, not
  // here — see `validateDaKeySetEncoding`. Config load only rejects values that
  // are malformed no matter what the policy is.
  const daCommitteeHex = yield* Config.string("DA_COMMITTEE_HEX").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) =>
      validateDaKeySetEncoding(
        value,
        VERIFICATION_KEY_HEX_LENGTH,
        "DA_COMMITTEE_HEX",
        "packed 32-byte verification keys",
      ),
    ),
  );
  const daThreshold = yield* Config.string("DA_THRESHOLD").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) => {
      const trimmed = value.trim();
      if (trimmed.length === 0) {
        return null;
      }
      const threshold = BigInt(trimmed);
      if (threshold <= 0n) {
        throw new Error(
          `DA_THRESHOLD must be a positive integer, received ${threshold.toString()}`,
        );
      }
      return threshold;
    }),
  );
  const daOwnersHex = yield* Config.string("DA_OWNERS_HEX").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) =>
      validateDaKeySetEncoding(
        value,
        VERIFICATION_KEY_HASH_HEX_LENGTH,
        "DA_OWNERS_HEX",
        "packed 28-byte payment key hashes",
      ),
    ),
  );
  const daCosignerSeedPhrase = yield* Config.string(
    "DA_COSIGNER_SEED_PHRASE",
  ).pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) => {
      const trimmed = value.trim();
      if (trimmed.length > 0) {
        // Derive once here so a malformed seed fails config load with a
        // ConfigError, the way the operator and reference-script seeds already
        // do, rather than surfacing later as an untyped defect inside
        // bootstrap or attestation signing.
        try {
          walletFromSeed(trimmed, { network });
        } catch (cause) {
          throw new Error(
            `DA_COSIGNER_SEED_PHRASE is not a valid wallet seed phrase: ${String(cause)}`,
          );
        }
      }
      return trimmed;
    }),
  );
  const daHardeningConfig = readDaHardeningConfig();
  const waitBetweenDepositUTxOFetches = yield* Config.integer(
    "WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES",
  ).pipe(Config.withDefault(10000));
  const promMetricsPort = yield* Config.integer("PROM_METRICS_PORT").pipe(
    Config.withDefault(9464),
  );
  const oltpExporterUrl = yield* Config.string("OLTP_EXPORTER_URL").pipe(
    Config.withDefault("http://0.0.0.0:4318/v1/traces"),
  );
  const postgresHost = yield* Config.string("POSTGRES_HOST").pipe(
    Config.withDefault("postgres"),
  ); // service name
  const postgresPort = yield* Config.integer("POSTGRES_PORT").pipe(
    Config.withDefault(5432),
  );
  const postgresPassword = yield* Config.string("POSTGRES_PASSWORD").pipe(
    Config.withDefault("postgres"),
  );
  const postgresDb = yield* Config.string("POSTGRES_DB").pipe(
    Config.withDefault("midgard"),
  );
  const postgresUser = yield* Config.string("POSTGRES_USER").pipe(
    Config.withDefault("postgres"),
  );
  const postgresAdmissionPoolSize = yield* Config.integer(
    "POSTGRES_ADMISSION_POOL_SIZE",
  ).pipe(
    Config.withDefault(10),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "POSTGRES_ADMISSION_POOL_SIZE must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const postgresBatchPoolSize = yield* Config.integer(
    "POSTGRES_BATCH_POOL_SIZE",
  ).pipe(
    Config.withDefault(20),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "POSTGRES_BATCH_POOL_SIZE must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const postgresWorkerPoolSize = yield* Config.integer(
    "POSTGRES_WORKER_POOL_SIZE",
  ).pipe(
    Config.withDefault(10),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "POSTGRES_WORKER_POOL_SIZE must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const admissionBacklogRefreshMs = yield* Config.integer(
    "ADMISSION_BACKLOG_REFRESH_MS",
  ).pipe(
    Config.withDefault(500),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "ADMISSION_BACKLOG_REFRESH_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const mempoolRetrievePageSize = yield* Config.integer(
    "MEMPOOL_RETRIEVE_PAGE_SIZE",
  ).pipe(
    Config.withDefault(20_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "MEMPOOL_RETRIEVE_PAGE_SIZE must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const writeBehindFlushIntervalMs = yield* Config.integer(
    "WRITE_BEHIND_FLUSH_INTERVAL_MS",
  ).pipe(
    Config.withDefault(100),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "WRITE_BEHIND_FLUSH_INTERVAL_MS must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const writeBehindMaxBatch = yield* Config.integer(
    "WRITE_BEHIND_MAX_BATCH",
  ).pipe(
    Config.withDefault(1_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "WRITE_BEHIND_MAX_BATCH must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const writeBehindQueueCapacity = yield* Config.integer(
    "WRITE_BEHIND_QUEUE_CAPACITY",
  ).pipe(
    Config.withDefault(50_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value <= 0) {
        throw new Error(
          "WRITE_BEHIND_QUEUE_CAPACITY must be a positive safe integer",
        );
      }
      return value;
    }),
  );
  const mpfEngine = yield* Config.literal(
    "legacy",
    "overlay",
    "event_flat",
    "architecture_g",
  )("MPF_ENGINE").pipe(Config.withDefault("legacy"));
  if (speculativeCommitBuild && mpfEngine === "legacy") {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Speculative commit building requires an overlay-capable MPF engine",
        cause: `MPF_ENGINE=${mpfEngine}`,
        fieldsAndValues: [
          ["SPECULATIVE_COMMIT_BUILD", "true"],
          ["MPF_ENGINE", mpfEngine],
        ],
      }),
    );
  }
  const mpfScratchBuild = yield* Config.literal(
    "insert",
    "fromlist",
  )("MPF_SCRATCH_BUILD").pipe(Config.withDefault("insert"));
  const mpfPathHydrationMode = yield* Config.literal(
    "whole_block",
    "chunked",
    "chunked_arena",
  )("MPF_PATH_HYDRATION_MODE").pipe(Config.withDefault("whole_block"));
  if (mpfPathHydrationMode !== "whole_block" && mpfEngine === "legacy") {
    return yield* Effect.fail(
      new ConfigError({
        message: "Chunked MPF hydration requires an overlay-capable MPF engine",
        cause: `MPF_ENGINE=${mpfEngine}`,
        fieldsAndValues: [
          ["MPF_PATH_HYDRATION_MODE", mpfPathHydrationMode],
          ["MPF_ENGINE", mpfEngine],
        ],
      }),
    );
  }
  if (mpfEngine === "event_flat" && mpfPathHydrationMode !== "chunked_arena") {
    return yield* Effect.fail(
      new ConfigError({
        message: "The event-flat MPF engine requires chunked_arena hydration",
        cause: `MPF_PATH_HYDRATION_MODE=${mpfPathHydrationMode}`,
        fieldsAndValues: [
          ["MPF_ENGINE", mpfEngine],
          ["MPF_PATH_HYDRATION_MODE", mpfPathHydrationMode],
        ],
      }),
    );
  }
  const mpfHydrationChunkOps = yield* positiveSafeIntegerConfig(
    "MPF_HYDRATION_CHUNK_OPS",
    512,
  );
  const mpfRetainHydratedDepth = yield* Config.integer(
    "MPF_RETAIN_HYDRATED_DEPTH",
  ).pipe(
    Config.withDefault(2),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0 || value > 8) {
        throw new Error(
          "MPF_RETAIN_HYDRATED_DEPTH must be a safe integer between 0 and 8",
        );
      }
      return value;
    }),
  );
  const mpfOverlaySpillBytes = yield* positiveSafeIntegerConfig(
    "MPF_OVERLAY_SPILL_BYTES",
    512 * 1024 * 1024,
  );
  const mpfPayloadRootCheck = yield* Config.literal(
    "every_block",
    "periodic",
    "off",
  )("MPF_PAYLOAD_ROOT_CHECK").pipe(Config.withDefault("every_block"));
  const mpfPayloadAuditIntervalBlocks = yield* positiveSafeIntegerConfig(
    "MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS",
    500,
  );
  const mpfPayloadAuditIntervalMs = yield* positiveSafeIntegerConfig(
    "MPF_PAYLOAD_AUDIT_INTERVAL_MS",
    6 * 60 * 60 * 1000,
  );
  const mpfParallelRoots = yield* Config.boolean("MPF_PARALLEL_ROOTS").pipe(
    Config.withDefault(false),
  );
  const mpfRootWorkers = yield* positiveSafeIntegerConfig(
    "MPF_ROOT_WORKERS",
    Math.max(1, Math.min(4, availableParallelism() - 2)),
  );
  const mpfParallelRootMinEntries = yield* positiveSafeIntegerConfig(
    "MPF_PARALLEL_ROOT_MIN_ENTRIES",
    5_000,
  );
  const commitMaxL2TxCount = yield* positiveSafeIntegerConfig(
    "COMMIT_MAX_L2_TX_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount) {
        throw new Error(
          `COMMIT_MAX_L2_TX_COUNT must be <= ${MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount.toString()}`,
        );
      }
      return value;
    }),
  );
  const commitMaxLedgerOpCount = yield* positiveSafeIntegerConfig(
    "COMMIT_MAX_LEDGER_OP_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount) {
        throw new Error(
          `COMMIT_MAX_LEDGER_OP_COUNT must be <= ${MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount.toString()}`,
        );
      }
      return value;
    }),
  );
  const commitMaxTransitionStepCount = yield* positiveSafeIntegerConfig(
    "COMMIT_MAX_TRANSITION_STEP_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount) {
        throw new Error(
          `COMMIT_MAX_TRANSITION_STEP_COUNT must be <= ${MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount.toString()}`,
        );
      }
      return value;
    }),
  );
  const commitBuildCostModel = yield* Config.literal(
    "static",
    "ewma",
  )("COMMIT_BUILD_COST_MODEL").pipe(Config.withDefault("static"));
  const commitBuildEwmaAlpha = yield* positiveFiniteNumberConfig(
    "COMMIT_BUILD_EWMA_ALPHA",
    0.2,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > 1) {
        throw new Error("COMMIT_BUILD_EWMA_ALPHA must be <= 1");
      }
      return value;
    }),
  );
  const commitBuildEwmaSafetyFactor = yield* positiveFiniteNumberConfig(
    "COMMIT_BUILD_EWMA_SAFETY_FACTOR",
    1.5,
  );
  const mpfRecordCorpus = yield* Config.string("MPF_RECORD_CORPUS").pipe(
    Config.withDefault(""),
  );
  const ledgerMpfDbPath = yield* Config.string("LEDGER_MPF_DB_PATH").pipe(
    Config.withDefault("midgard-ledger-mpf-db"),
  );
  const transactionsMpfDbPath = yield* Config.string(
    "TRANSACTIONS_MPF_DB_PATH",
  ).pipe(Config.withDefault("midgard-transactions-mpf-db"));
  const mpfNativeOwnerBinaryPath = yield* Config.string(
    "MPF_NATIVE_OWNER_BINARY_PATH",
  ).pipe(
    Config.withDefault(
      "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    ),
  );
  const mpfNativeOwnerBinarySha256 = yield* Config.string(
    "MPF_NATIVE_OWNER_BINARY_SHA256",
  ).pipe(Config.withDefault(""));
  if (
    mpfEngine === "architecture_g" &&
    !/^[0-9a-f]{64}$/.test(mpfNativeOwnerBinarySha256)
  ) {
    return yield* Effect.fail(
      new ConfigError({
        message:
          "Architecture G requires an explicitly pinned native owner binary SHA-256",
        cause: "MPF_NATIVE_OWNER_BINARY_SHA256 is absent or non-canonical",
        fieldsAndValues: [
          ["MPF_ENGINE", mpfEngine],
          ["MPF_NATIVE_OWNER_BINARY_PATH", mpfNativeOwnerBinaryPath],
          ["MPF_NATIVE_OWNER_BINARY_SHA256", mpfNativeOwnerBinarySha256],
        ],
      }),
    );
  }
  const mpfNativeOwnerSidecarPath = yield* Config.string(
    "MPF_NATIVE_OWNER_SIDECAR_PATH",
  ).pipe(Config.withDefault(`${ledgerMpfDbPath}.architecture-g.sidecar`));
  const mpfNativeOwnerMaxFrameBytes = yield* positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_MAX_FRAME_BYTES",
    64 * 1024 * 1024,
  );
  const mpfNativeOwnerMaxChunkBytes = yield* positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_MAX_CHUNK_BYTES",
    16 * 1024 * 1024,
  );
  if (
    mpfNativeOwnerMaxFrameBytes > 64 * 1024 * 1024 ||
    mpfNativeOwnerMaxChunkBytes > 16 * 1024 * 1024
  ) {
    return yield* Effect.fail(
      new ConfigError({
        message: "Architecture G RPC caps exceed the compiled native limits",
        cause: `chunk_bytes=${mpfNativeOwnerMaxChunkBytes.toString()},frame_bytes=${mpfNativeOwnerMaxFrameBytes.toString()}`,
        fieldsAndValues: [
          [
            "MPF_NATIVE_OWNER_MAX_CHUNK_BYTES",
            mpfNativeOwnerMaxChunkBytes.toString(),
          ],
          [
            "MPF_NATIVE_OWNER_MAX_FRAME_BYTES",
            mpfNativeOwnerMaxFrameBytes.toString(),
          ],
        ],
      }),
    );
  }
  if (mpfNativeOwnerMaxChunkBytes > mpfNativeOwnerMaxFrameBytes - 68) {
    return yield* Effect.fail(
      new ConfigError({
        message: "Architecture G chunk cap must fit inside the RPC frame cap",
        cause: `chunk_bytes=${mpfNativeOwnerMaxChunkBytes.toString()},frame_bytes=${mpfNativeOwnerMaxFrameBytes.toString()}`,
        fieldsAndValues: [
          [
            "MPF_NATIVE_OWNER_MAX_CHUNK_BYTES",
            mpfNativeOwnerMaxChunkBytes.toString(),
          ],
          [
            "MPF_NATIVE_OWNER_MAX_FRAME_BYTES",
            mpfNativeOwnerMaxFrameBytes.toString(),
          ],
        ],
      }),
    );
  }
  const mpfNativeOwnerRequestTimeoutMs = yield* positiveSafeIntegerConfig(
    "MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS",
    120_000,
  );
  const mpfNativeOwnerRestartLimit = yield* Config.integer(
    "MPF_NATIVE_OWNER_RESTART_LIMIT",
  ).pipe(
    Config.withDefault(3),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0) {
        throw new Error(
          "MPF_NATIVE_OWNER_RESTART_LIMIT must be a non-negative safe integer",
        );
      }
      return value;
    }),
  );
  const seedA = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A");
  const seedB = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B");
  const seedC = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C");
  const addressA = walletFromSeed(seedA, { network }).address;
  const addressB = walletFromSeed(seedB, { network }).address;
  const addressC = walletFromSeed(seedC, { network }).address;

  const genesisUtxosByWallet = {
    A: [
      {
        txHash:
          "bb217abaca60fc0ca68c1555eca6a96d2478547818ae76ce6836133f3cc546e0",
        outputIndex: 1,
        address: addressA,
        assets: {
          lovelace: 4_027_026_465n,
          // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
          //   BigInt("10000"),
        },
      },
      {
        txHash:
          "c7c0973c6bbf1a04a9f306da7814b4fa564db649bf48b0bd93c273bd03143547",
        outputIndex: 0,
        address: addressA,
        assets: {
          lovelace: 3_289_566n,
          // "5c677ba4dd295d9286e0e22786fea9ed735a6ae9c07e7a45ae4d95c84372696d696e616c50756e6b73204c6f6f74":
          //   BigInt("1"),
        },
      },
    ],
    B: [
      {
        txHash:
          "d1a25b8e9c3b985d9d2f0a5f2e6ca7efa1c43b10f2c0b61f29e4a2cd8142b09e",
        outputIndex: 0,
        address: addressB,
        assets: {
          lovelace: 200n,
        },
      },
      {
        txHash:
          "ea0f3c47bf18b02e9deb4e3a1239d8b263d765c4f7a3d12a9f62e8775e8c6141",
        outputIndex: 1,
        address: addressB,
        assets: {
          lovelace: 1_500n,
        },
      },
      {
        txHash:
          "f40b9f6a507af50aad4ccf6c15157b6d05c7affe23ec55cf4109cc2549c97a37",
        outputIndex: 2,
        address: addressB,
        assets: {
          lovelace: 125_243n,
        },
      },
    ],
    C: [
      {
        txHash:
          "8e32d18c07cba2b65577bc829a9875e2fc3cdb554d5b0abbb3d4e3a71a3e3e3d",
        outputIndex: 0,
        address: addressC,
        assets: {
          lovelace: 300n,
          // "25561d09e55d60b64525b9cdb3cfbec23c94c0634320fec2eaddde584c616365436f696e33":
          //   BigInt("15"),
        },
      },
    ],
  } as const;
  const genesisUtxos: UTxO[] = [
    ...genesisUtxosByWallet.A,
    ...genesisUtxosByWallet.B,
    ...genesisUtxosByWallet.C,
  ];

  return {
    L1_PROVIDER: provider,
    L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: l1ProviderPreflightTimeoutMs,
    L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: l1ProviderRateLimitCooldownMs,
    L1_RECENT_TX_VISIBILITY_TIMEOUT_MS: l1RecentTxVisibilityTimeoutMs,
    L1_RECENT_TX_404_MAX_DELAY_MS: l1RecentTx404MaxDelayMs,
    L1_OGMIOS_KEY: ogmiosKey,
    L1_KUPO_KEY: kupoKey,
    L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: operatorSeedPhraseForMergeTx,
    L1_REFERENCE_SCRIPT_SEED_PHRASE: referenceScriptSeedPhrase,
    L1_REFERENCE_SCRIPT_ADDRESS: referenceScriptAddress,
    L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: referenceScriptDeployAddress,
    REFERENCE_SCRIPT_AUTH_TIMELOCK_MS: referenceScriptAuthTimelockMs,
    REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS: referenceScriptAuthMinRemainingMs,
    NETWORK: network,
    MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: deploymentEconomicsProfile,
    PORT: port,
    WAIT_BETWEEN_BLOCK_COMMITMENT: waitBetweenBlockCommitment,
    WAIT_BETWEEN_BLOCK_CONFIRMATION: waitBetweenBlockConfirmation,
    SPECULATIVE_COMMIT_BUILD: speculativeCommitBuild,
    SPECULATIVE_REBUILD_MAX_ATTEMPTS: speculativeRebuildMaxAttempts,
    USER_EVENT_BARRIER_REFRESH_MS: userEventBarrierRefreshMs,
    USER_EVENT_BARRIER_MAX_STALENESS_MS: userEventBarrierMaxStalenessMs,
    USER_EVENT_INCLUSION_DEADLINE_MS: userEventInclusionDeadlineMs,
    BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: blockConfirmationAwaitTimeoutMs,
    BLOCK_CONFIRMATION_AWAIT_RETRIES: blockConfirmationAwaitRetries,
    UNCONFIRMED_BLOCK_MAX_AGE_MS: unconfirmedBlockMaxAgeMs,
    WAIT_BETWEEN_MERGE_TXS: waitBetweenMergeTxs,
    MIN_QUEUE_LENGTH_FOR_MERGING: minQueueLengthForMerging,
    VALIDATION_BATCH_SIZE: validationBatchSize,
    VALIDATION_BATCH_HARD_CAP: validationBatchHardCap,
    VALIDATION_MIN_BATCH: validationMinBatch,
    VALIDATION_MAX_QUEUE_AGE_MS: validationMaxQueueAgeMs,
    VALIDATION_PHASE_A_CONCURRENCY: validationPhaseAConcurrency,
    VALIDATION_G4_BUCKET_CONCURRENCY: validationG4BucketConcurrency,
    VALIDATION_STRICTNESS_PROFILE: validationStrictnessProfile,
    VALIDATION_WORKER_POOL_SIZE: validationWorkerPoolSize,
    VALIDATION_WORKER_CHUNK_SIZE: validationWorkerChunkSize,
    VALIDATION_WORKER_INLINE_THRESHOLD: validationWorkerInlineThreshold,
    VALIDATION_WORKER_JOB_TIMEOUT_MS: validationWorkerJobTimeoutMs,
    VALIDATION_WORKER_NODE_ED25519: validationWorkerNodeEd25519,
    VALIDATION_DRAIN_LOOPS: validationDrainLoops,
    VALIDATION_UPLC_IN_WORKERS: validationUplcInWorkers,
    VALIDATION_LEDGER_DELTA_LOG_MAX: validationLedgerDeltaLogMax,
    TX_QUEUE_POLL_INTERVAL_MS: txQueuePollIntervalMs,
    MIN_FEE_A: minFeeA,
    MIN_FEE_B: minFeeB,
    RUN_GENESIS_ON_STARTUP: runGenesisOnStartup,
    ADMIN_API_KEY: adminApiKey,
    MAX_DURABLE_ADMISSION_BACKLOG: maxDurableAdmissionBacklog,
    MAX_DURABLE_ADMISSION_BACKLOG_BYTES: maxDurableAdmissionBacklogBytes,
    SUBMIT_INGRESS_MAX_CONCURRENCY: submitIngressMaxConcurrency,
    SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES: submitIngressMaxInFlightBytes,
    CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: cekProgramMaterialStoreMaxBytes,
    MAX_SUBMIT_TX_CBOR_BYTES: maxSubmitTxCborBytes,
    READINESS_MAX_HEARTBEAT_AGE_MS: readinessMaxHeartbeatAgeMs,
    READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS:
      readinessL1ProviderEvidenceMaxAgeMs,
    READINESS_MAX_DURABLE_ADMISSION_BACKLOG:
      readinessMaxDurableAdmissionBacklog,
    READINESS_MAX_DURABLE_ADMISSION_AGE_MS: readinessMaxDurableAdmissionAgeMs,
    STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS:
      startupProtocolStatusQueryMaxAttempts,
    STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS:
      startupProtocolStatusQueryRetryDelayMs,
    VALIDATION_LEASE_MS: validationLeaseMs,
    VALIDATION_RETRY_BACKOFF_BASE_MS: validationRetryBackoffBaseMs,
    VALIDATION_RETRY_BACKOFF_MAX_MS: validationRetryBackoffMaxMs,
    VALIDATION_EXPIRED_LEASE_READINESS_THRESHOLD:
      validationExpiredLeaseReadinessThreshold,
    STATE_QUEUE_MUTATION_LEASE_TTL_MS: stateQueueMutationLeaseTtlMs,
    STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS:
      stateQueueMutationLeaseRenewIntervalMs,
    STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS:
      stateQueueMutationLeaseStaleGraceMs,
    STATE_QUEUE_CORRECTION_FINALITY_DEPTH: stateQueueCorrectionFinalityDepth,
    RETENTION_DAYS: retentionDays,
    WAIT_BETWEEN_RETENTION_SWEEPS: waitBetweenRetentionSweeps,
    HUB_ORACLE_ONE_SHOT_TX_HASH: hubOracleOneShotTxHash,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: hubOracleOneShotOutputIndex,
    OPERATOR_REQUIRED_BOND_LOVELACE: operatorRequiredBondLovelace,
    OPERATOR_SLASHING_PENALTY_LOVELACE: operatorSlashingPenaltyLovelace,
    DA_COMMITTEE_HEX: daCommitteeHex,
    DA_THRESHOLD: daThreshold,
    DA_OWNERS_HEX: daOwnersHex,
    DA_COSIGNER_SEED_PHRASE: daCosignerSeedPhrase,
    MIDGARD_DA_PAYLOAD_ENVELOPE: daHardeningConfig.envelopeMode,
    MIDGARD_DA_ZSTD_LEVEL: daHardeningConfig.zstdLevel,
    MIDGARD_DA_PUBLISH_CONCURRENCY: daHardeningConfig.publishConcurrency,
    MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS:
      daHardeningConfig.reconcileIntervalMs,
    MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS: daHardeningConfig.retryBackoffMs,
    MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS:
      daHardeningConfig.retryBackoffMaxMs,
    WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: waitBetweenDepositUTxOFetches,
    PROM_METRICS_PORT: promMetricsPort,
    OLTP_EXPORTER_URL: oltpExporterUrl,
    POSTGRES_HOST: postgresHost,
    POSTGRES_PORT: postgresPort,
    POSTGRES_PASSWORD: postgresPassword,
    POSTGRES_DB: postgresDb,
    POSTGRES_USER: postgresUser,
    POSTGRES_ADMISSION_POOL_SIZE: postgresAdmissionPoolSize,
    POSTGRES_BATCH_POOL_SIZE: postgresBatchPoolSize,
    POSTGRES_WORKER_POOL_SIZE: postgresWorkerPoolSize,
    ADMISSION_BACKLOG_REFRESH_MS: admissionBacklogRefreshMs,
    MEMPOOL_RETRIEVE_PAGE_SIZE: mempoolRetrievePageSize,
    WRITE_BEHIND_FLUSH_INTERVAL_MS: writeBehindFlushIntervalMs,
    WRITE_BEHIND_MAX_BATCH: writeBehindMaxBatch,
    WRITE_BEHIND_QUEUE_CAPACITY: writeBehindQueueCapacity,
    MPF_ENGINE: mpfEngine,
    MPF_SCRATCH_BUILD: mpfScratchBuild,
    MPF_PATH_HYDRATION_MODE: mpfPathHydrationMode,
    MPF_HYDRATION_CHUNK_OPS: mpfHydrationChunkOps,
    MPF_RETAIN_HYDRATED_DEPTH: mpfRetainHydratedDepth,
    MPF_OVERLAY_SPILL_BYTES: mpfOverlaySpillBytes,
    MPF_PAYLOAD_ROOT_CHECK: mpfPayloadRootCheck,
    MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS: mpfPayloadAuditIntervalBlocks,
    MPF_PAYLOAD_AUDIT_INTERVAL_MS: mpfPayloadAuditIntervalMs,
    MPF_PARALLEL_ROOTS: mpfParallelRoots,
    MPF_ROOT_WORKERS: mpfRootWorkers,
    MPF_PARALLEL_ROOT_MIN_ENTRIES: mpfParallelRootMinEntries,
    COMMIT_MAX_L2_TX_COUNT: commitMaxL2TxCount,
    COMMIT_MAX_LEDGER_OP_COUNT: commitMaxLedgerOpCount,
    COMMIT_MAX_TRANSITION_STEP_COUNT: commitMaxTransitionStepCount,
    COMMIT_BUILD_COST_MODEL: commitBuildCostModel,
    COMMIT_BUILD_EWMA_ALPHA: commitBuildEwmaAlpha,
    COMMIT_BUILD_EWMA_SAFETY_FACTOR: commitBuildEwmaSafetyFactor,
    MPF_RECORD_CORPUS: mpfRecordCorpus,
    MPF_NATIVE_OWNER_BINARY_PATH: mpfNativeOwnerBinaryPath,
    MPF_NATIVE_OWNER_BINARY_SHA256: mpfNativeOwnerBinarySha256,
    MPF_NATIVE_OWNER_SIDECAR_PATH: mpfNativeOwnerSidecarPath,
    MPF_NATIVE_OWNER_MAX_FRAME_BYTES: mpfNativeOwnerMaxFrameBytes,
    MPF_NATIVE_OWNER_MAX_CHUNK_BYTES: mpfNativeOwnerMaxChunkBytes,
    MPF_NATIVE_OWNER_REQUEST_TIMEOUT_MS: mpfNativeOwnerRequestTimeoutMs,
    MPF_NATIVE_OWNER_RESTART_LIMIT: mpfNativeOwnerRestartLimit,
    LEDGER_MPF_DB_PATH: ledgerMpfDbPath,
    TRANSACTIONS_MPF_DB_PATH: transactionsMpfDbPath,
    GENESIS_UTXOS: network === "Mainnet" ? [] : genesisUtxos,
    GENESIS_UTXOS_BY_WALLET:
      network === "Mainnet" ? { A: [], B: [], C: [] } : genesisUtxosByWallet,
  };
}).pipe(Effect.orDie);

/**
 * Effect service carrying the decoded node configuration.
 */
export class NodeConfig extends Context.Tag("NodeConfig")<
  NodeConfig,
  NodeConfigDep
>() {
  static readonly layer = Layer.effect(NodeConfig, makeConfig);
}

/**
 * Tagged configuration error enriched with the relevant field/value pairs.
 */
export class ConfigError extends Data.TaggedError("ConfigError")<
  SDK.GenericErrorFields & {
    readonly fieldsAndValues: [string, string][];
  }
> {}
