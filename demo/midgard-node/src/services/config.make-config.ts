import { availableParallelism } from "node:os";

import { resolveL1ViewFatalMs } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { requireSelectedDeploymentProfile } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
} from "@al-ft/midgard-sdk";
import { Config, Context, Data, Effect, Layer, Option } from "effect";

import { readDaHardeningConfig } from "../da/hardening-config.js";
import {
  VERIFICATION_KEY_HASH_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "../da/local-signers.js";
import { validateRetentionDays } from "../database/retention-policy.js";
import { historyCommitHorizonLagConfig } from "./config.history-commit-horizon-lag.js";
import { hubOracleOriginConfig } from "./config.hub-oracle-origin.js";
import { l1ContentSourcesConfig } from "./config.l1-content-sources.js";
import { nodeBehindConfig } from "./config.node-behind.js";
import {
  boundedValidationInteger,
  CEK_PROGRAM_MATERIAL_MIN_STORE_BYTES,
  type NodeConfigDep,
  positiveFiniteNumberConfig,
  positiveSafeIntegerConfig,
  requiredSeedPhrase,
  resolveValidationWorkerPoolSize,
  seedPhraseAddress,
  validateDaKeySetEncoding,
  validateSeedPhrase,
} from "./config.node-config-dep.js";
import {
  NATIVE_LEDGER_SETTING_NAMES,
  parseNativeLedgerSettings,
} from "./native-ledger.js";

/**
 * Loads and normalizes the node's runtime configuration from environment
 * variables.
 */
const makeConfig = Effect.gen(function* () {
  const provider = yield* Config.literal("Kupmios")("L1_PROVIDER");
  const ogmiosKey = yield* Config.string("L1_OGMIOS_KEY");
  const kupoKey = yield* Config.string("L1_KUPO_KEY");
  const nativeLedgerValues = yield* Config.all(
    Object.fromEntries(
      NATIVE_LEDGER_SETTING_NAMES.map((name) => [
        name,
        Config.string(name).pipe(Config.withDefault("")),
      ]),
    ) as Record<
      (typeof NATIVE_LEDGER_SETTING_NAMES)[number],
      Config.Config<string>
    >,
  );
  const nativeLedger = yield* Effect.try(() =>
    parseNativeLedgerSettings(nativeLedgerValues),
  );
  const historyGenesis = yield* Config.string(
    "L1_HISTORY_GENESIS_LOSSLESS_SHA256",
  ).pipe(Config.withDefault(""));
  const operatorSeedPhrase = yield* requiredSeedPhrase(
    "L1_OPERATOR_SEED_PHRASE",
  );
  const operatorSeedPhraseForMergeTx = yield* requiredSeedPhrase(
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  );
  const settlementSeedPhrase = yield* Config.string(
    "L1_SETTLEMENT_SEED_PHRASE",
  ).pipe(Config.withDefault(""));
  const network = yield* Config.literal(
    "Mainnet",
    "Preprod",
    "Preview",
    "Custom",
  )("NETWORK");
  const deploymentProfile = yield* Config.string(
    "MIDGARD_DEPLOYMENT_PROFILE",
  ).pipe(Config.mapAttempt(requireSelectedDeploymentProfile));
  if (network !== deploymentProfile.network) {
    return yield* Effect.fail(
      new Error("NETWORK must match the compiled deployment profile"),
    );
  }
  const deploymentEconomics = deploymentProfile.economics;
  const deploymentEconomicsProfile = deploymentEconomics.profile;
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
  ).pipe(
    Config.withDefault(operatorSeedPhrase),
    Config.mapAttempt((value) =>
      validateSeedPhrase("L1_REFERENCE_SCRIPT_SEED_PHRASE", value),
    ),
  );
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
  const derivedReferenceScriptAddress = seedPhraseAddress(
    referenceScriptSeedPhrase,
    network,
  );
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
  const operatorWatchdogEnabled = yield* Config.boolean(
    "OPERATOR_WATCHDOG_ENABLED",
  ).pipe(Config.withDefault(true));
  const operatorWatchdogPatienceMs = yield* Config.integer(
    "OPERATOR_WATCHDOG_PATIENCE_MS",
  ).pipe(
    Config.withDefault(120_000),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0) {
        throw new Error(
          "OPERATOR_WATCHDOG_PATIENCE_MS must be a non-negative safe integer",
        );
      }
      return value;
    }),
  );
  const historyCommitHorizonLag = yield* historyCommitHorizonLagConfig;
  const nodeBehind = yield* nodeBehindConfig;
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
    Config.withDefault(DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth),
    Config.mapAttempt((value) => {
      if (value !== DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth) {
        throw new Error(
          `STATE_QUEUE_CORRECTION_FINALITY_DEPTH must equal the deployment profile value ${DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth.toString()}`,
        );
      }
      return value;
    }),
  );
  // B5: no default; unset means the verified manifest's retention window. A
  // shorter explicit value refuses startup in
  // assertDeploymentManifestMatchesConfig. DA payload pruning ignores it.
  const retentionDays = yield* Config.option(
    Config.integer("RETENTION_DAYS"),
  ).pipe(
    Config.mapAttempt((value) =>
      Option.isNone(value) ? undefined : validateRetentionDays(value.value),
    ),
  );
  const waitBetweenRetentionSweeps = yield* Config.integer(
    "WAIT_BETWEEN_RETENTION_SWEEPS",
  ).pipe(
    Config.withDefault(
      Math.min(900_000, Math.floor(Number(SDK.DA_ATTESTATION_TIMEOUT_MS) / 4)),
    ),
  );
  const l1ViewFatalMs = yield* Config.option(
    Config.string("L1_VIEW_FATAL_MS"),
  ).pipe(
    Config.mapAttempt((value) =>
      resolveL1ViewFatalMs({
        value: Option.getOrUndefined(value),
        defaultMs: Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
        pollIntervalMs: waitBetweenRetentionSweeps,
        fieldName: "L1_VIEW_FATAL_MS",
      }),
    ),
  );
  const hubOracleOrigin = yield* hubOracleOriginConfig;
  const l1ContentSources = yield* l1ContentSourcesConfig;
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
          seedPhraseAddress(trimmed, network);
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
  const mpfScratchBuild = yield* Config.literal(
    "insert",
    "fromlist",
  )("MPF_SCRATCH_BUILD").pipe(Config.withDefault("insert"));
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

  return {
    L1_PROVIDER: provider,
    L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: l1ProviderPreflightTimeoutMs,
    L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: l1ProviderRateLimitCooldownMs,
    L1_RECENT_TX_VISIBILITY_TIMEOUT_MS: l1RecentTxVisibilityTimeoutMs,
    L1_RECENT_TX_404_MAX_DELAY_MS: l1RecentTx404MaxDelayMs,
    L1_OGMIOS_KEY: ogmiosKey,
    L1_KUPO_KEY: kupoKey,
    L1_NATIVE_LEDGER: nativeLedger,
    L1_HISTORY_GENESIS_LOSSLESS_SHA256: historyGenesis,
    L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: operatorSeedPhraseForMergeTx,
    L1_SETTLEMENT_SEED_PHRASE: settlementSeedPhrase,
    L1_REFERENCE_SCRIPT_SEED_PHRASE: referenceScriptSeedPhrase,
    L1_REFERENCE_SCRIPT_ADDRESS: referenceScriptAddress,
    L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: referenceScriptDeployAddress,
    REFERENCE_SCRIPT_AUTH_TIMELOCK_MS: referenceScriptAuthTimelockMs,
    REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS: referenceScriptAuthMinRemainingMs,
    NETWORK: network,
    DEPLOYMENT_ECONOMICS_PROFILE: deploymentEconomicsProfile,
    PORT: port,
    WAIT_BETWEEN_BLOCK_COMMITMENT: waitBetweenBlockCommitment,
    WAIT_BETWEEN_BLOCK_CONFIRMATION: waitBetweenBlockConfirmation,
    OPERATOR_WATCHDOG_ENABLED: operatorWatchdogEnabled,
    OPERATOR_WATCHDOG_PATIENCE_MS: operatorWatchdogPatienceMs,
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
    ...historyCommitHorizonLag,
    ...nodeBehind,
    RETENTION_DAYS: retentionDays,
    WAIT_BETWEEN_RETENTION_SWEEPS: waitBetweenRetentionSweeps,
    L1_VIEW_FATAL_MS: l1ViewFatalMs,
    ...hubOracleOrigin,
    ...l1ContentSources,
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
    MPF_SCRATCH_BUILD: mpfScratchBuild,
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
    // Atomic initialization commits an empty ledger on every network.
    // Isolated harnesses may inject an explicitly authenticated genesis fixture.
    GENESIS_UTXOS: [],
    GENESIS_UTXOS_BY_WALLET: { A: [], B: [], C: [] },
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
