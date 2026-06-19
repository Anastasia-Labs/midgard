import * as SDK from "@al-ft/midgard-sdk";
import { Network, UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Config, Context, Data, Effect, Layer } from "effect";

import { validateRetentionDays } from "@/database/retention-policy.js";
import {
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
} from "@al-ft/midgard-sdk";

/**
 * Configuration loading for the Midgard node process.
 *
 * This module centralizes environment-variable decoding, defaulting, and the
 * derived values that other services depend on. Keeping it in one place makes
 * production configuration easier to audit.
 */
type Provider = "Kupmios";

/**
 * Fully-decoded runtime configuration required by the node.
 */
type NodeConfigDep = {
  L1_PROVIDER: Provider;
  L1_PROVIDER_FAILOVER: readonly never[];
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
  PROTOCOL_PARAMETERS: SDK.ProtocolParameters;
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENT: number;
  WAIT_BETWEEN_BLOCK_CONFIRMATION: number;
  BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: number;
  BLOCK_CONFIRMATION_AWAIT_RETRIES: number;
  UNCONFIRMED_BLOCK_MAX_AGE_MS: number;
  WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: number;
  WAIT_BETWEEN_MERGE_TXS: number;
  MIN_QUEUE_LENGTH_FOR_MERGING: number;
  VALIDATION_BATCH_SIZE: number;
  VALIDATION_MAX_QUEUE_AGE_MS: number;
  VALIDATION_PHASE_A_CONCURRENCY: number;
  VALIDATION_G4_BUCKET_CONCURRENCY: number;
  VALIDATION_STRICTNESS_PROFILE: string;
  MIN_FEE_A: bigint;
  MIN_FEE_B: bigint;
  RUN_GENESIS_ON_STARTUP: boolean;
  ADMIN_API_KEY: string;
  MAX_SUBMIT_QUEUE_SIZE: number;
  MAX_DURABLE_ADMISSION_BACKLOG: number;
  MAX_SUBMIT_TX_CBOR_BYTES: number;
  READINESS_MAX_HEARTBEAT_AGE_MS: number;
  READINESS_MAX_QUEUE_DEPTH: number;
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
  RETENTION_DAYS: number;
  WAIT_BETWEEN_RETENTION_SWEEPS: number;
  HUB_ORACLE_ONE_SHOT_TX_HASH: string;
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  OPERATOR_REQUIRED_BOND_LOVELACE: bigint;
  OPERATOR_SLASHING_PENALTY_LOVELACE: bigint;
  DA_COMMITTEE_HEX: string;
  DA_THRESHOLD: bigint | null;
  PROM_METRICS_PORT: number;
  OLTP_EXPORTER_URL: string;
  POSTGRES_USER: string;
  POSTGRES_PASSWORD: string;
  POSTGRES_DB: string;
  POSTGRES_HOST: string;
  POSTGRES_PORT: number;
  LEDGER_MPF_DB_PATH: string;
  TRANSACTIONS_MPF_DB_PATH: string;
  GENESIS_UTXOS: UTxO[];
};

const rejectDeprecatedRootStoreEnvVars = Effect.sync(() => {
  const deprecated = [
    ["LEDGER_MPT_DB_PATH", "LEDGER_MPF_DB_PATH"],
    ["MEMPOOL_MPT_DB_PATH", "TRANSACTIONS_MPF_DB_PATH"],
    ["MEMPOOL_MPF_DB_PATH", "TRANSACTIONS_MPF_DB_PATH"],
  ] as const;
  const configuredDeprecated = deprecated.filter(
    ([name]) => process.env[name] !== undefined,
  );
  if (configuredDeprecated.length > 0) {
    throw new Error(
      configuredDeprecated
        .map(
          ([name, replacement]) =>
            `${name} is no longer supported; use ${replacement}`,
        )
        .join("; "),
    );
  }
});

/**
 * Loads and normalizes the node's runtime configuration from environment
 * variables.
 */
const makeConfig = Effect.gen(function* () {
  yield* rejectDeprecatedRootStoreEnvVars;
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
  const providerFailover = yield* Config.string("L1_PROVIDER_FAILOVER").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) => {
      if (value.trim().length > 0) {
        throw new Error(
          "L1_PROVIDER_FAILOVER is no longer supported for demo/midgard-node acceptance; use local Kupmios only.",
        );
      }
      return [];
    }),
  );
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
  ).pipe(Config.withDefault(10000));
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
  const validationBatchSize = yield* Config.integer(
    "VALIDATION_BATCH_SIZE",
  ).pipe(Config.withDefault(1000));
  const validationMaxQueueAgeMs = yield* Config.integer(
    "VALIDATION_MAX_QUEUE_AGE_MS",
  ).pipe(Config.withDefault(250));
  const validationPhaseAConcurrency = yield* Config.integer(
    "VALIDATION_PHASE_A_CONCURRENCY",
  ).pipe(Config.withDefault(32));
  const validationG4BucketConcurrency = yield* Config.integer(
    "VALIDATION_G4_BUCKET_CONCURRENCY",
  ).pipe(Config.withDefault(8));
  const validationStrictnessProfile = yield* Config.string(
    "VALIDATION_STRICTNESS_PROFILE",
  ).pipe(Config.withDefault("phase1_midgard"));
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
  const maxSubmitQueueSize = yield* Config.integer(
    "MAX_SUBMIT_QUEUE_SIZE",
  ).pipe(Config.withDefault(10_000));
  const maxDurableAdmissionBacklog = yield* Config.integer(
    "MAX_DURABLE_ADMISSION_BACKLOG",
  ).pipe(Config.withDefault(maxSubmitQueueSize));
  const maxSubmitTxCborBytes = yield* Config.integer(
    "MAX_SUBMIT_TX_CBOR_BYTES",
  ).pipe(Config.withDefault(32_768));
  const readinessMaxHeartbeatAgeMs = yield* Config.integer(
    "READINESS_MAX_HEARTBEAT_AGE_MS",
  ).pipe(Config.withDefault(120_000));
  const readinessMaxQueueDepth = yield* Config.integer(
    "READINESS_MAX_QUEUE_DEPTH",
  ).pipe(Config.withDefault(maxSubmitQueueSize));
  const readinessMaxDurableAdmissionBacklog = yield* Config.integer(
    "READINESS_MAX_DURABLE_ADMISSION_BACKLOG",
  ).pipe(Config.withDefault(readinessMaxQueueDepth));
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
  const validationRetryBackoffBaseMs = yield* Config.integer(
    "VALIDATION_RETRY_BACKOFF_BASE_MS",
  ).pipe(Config.withDefault(250));
  const validationRetryBackoffMaxMs = yield* Config.integer(
    "VALIDATION_RETRY_BACKOFF_MAX_MS",
  ).pipe(Config.withDefault(10_000));
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
  const retentionDays = yield* Config.integer("RETENTION_DAYS").pipe(
    Config.withDefault(0),
    Config.mapAttempt(validateRetentionDays),
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
    Config.withDefault("5000000"),
    Config.mapAttempt((value) => BigInt(value)),
  );
  const operatorSlashingPenaltyLovelace = yield* Config.string(
    "OPERATOR_SLASHING_PENALTY_LOVELACE",
  ).pipe(
    Config.withDefault("200000"),
    Config.mapAttempt((value) => BigInt(value)),
  );
  const daCommitteeHex = yield* Config.string("DA_COMMITTEE_HEX").pipe(
    Config.withDefault(""),
    Config.map((value) => value.trim().toLowerCase()),
  );
  const daThreshold = yield* Config.string("DA_THRESHOLD").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) => {
      const trimmed = value.trim();
      return trimmed.length === 0 ? null : BigInt(trimmed);
    }),
  );
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
  const ledgerMpfDbPath = yield* Config.string("LEDGER_MPF_DB_PATH").pipe(
    Config.withDefault("midgard-ledger-mpf-db"),
  );
  const transactionsMpfDbPath = yield* Config.string(
    "TRANSACTIONS_MPF_DB_PATH",
  ).pipe(Config.withDefault("midgard-transactions-mpf-db"));
  const seedA = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_A");
  const seedB = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_B");
  const seedC = yield* Config.string("TESTNET_GENESIS_WALLET_SEED_PHRASE_C");
  const addressA = walletFromSeed(seedA, { network }).address;
  const addressB = walletFromSeed(seedB, { network }).address;
  const addressC = walletFromSeed(seedC, { network }).address;

  const genesisUtxos: UTxO[] = [
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
  ];

  return {
    L1_PROVIDER: provider,
    L1_PROVIDER_FAILOVER: providerFailover,
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
    PROTOCOL_PARAMETERS: SDK.getProtocolParameters(network),
    PORT: port,
    WAIT_BETWEEN_BLOCK_COMMITMENT: waitBetweenBlockCommitment,
    WAIT_BETWEEN_BLOCK_CONFIRMATION: waitBetweenBlockConfirmation,
    BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: blockConfirmationAwaitTimeoutMs,
    BLOCK_CONFIRMATION_AWAIT_RETRIES: blockConfirmationAwaitRetries,
    UNCONFIRMED_BLOCK_MAX_AGE_MS: unconfirmedBlockMaxAgeMs,
    WAIT_BETWEEN_MERGE_TXS: waitBetweenMergeTxs,
    MIN_QUEUE_LENGTH_FOR_MERGING: minQueueLengthForMerging,
    VALIDATION_BATCH_SIZE: validationBatchSize,
    VALIDATION_MAX_QUEUE_AGE_MS: validationMaxQueueAgeMs,
    VALIDATION_PHASE_A_CONCURRENCY: validationPhaseAConcurrency,
    VALIDATION_G4_BUCKET_CONCURRENCY: validationG4BucketConcurrency,
    VALIDATION_STRICTNESS_PROFILE: validationStrictnessProfile,
    MIN_FEE_A: minFeeA,
    MIN_FEE_B: minFeeB,
    RUN_GENESIS_ON_STARTUP: runGenesisOnStartup,
    ADMIN_API_KEY: adminApiKey,
    MAX_SUBMIT_QUEUE_SIZE: maxSubmitQueueSize,
    MAX_DURABLE_ADMISSION_BACKLOG: maxDurableAdmissionBacklog,
    MAX_SUBMIT_TX_CBOR_BYTES: maxSubmitTxCborBytes,
    READINESS_MAX_HEARTBEAT_AGE_MS: readinessMaxHeartbeatAgeMs,
    READINESS_MAX_QUEUE_DEPTH: readinessMaxQueueDepth,
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
    RETENTION_DAYS: retentionDays,
    WAIT_BETWEEN_RETENTION_SWEEPS: waitBetweenRetentionSweeps,
    HUB_ORACLE_ONE_SHOT_TX_HASH: hubOracleOneShotTxHash,
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: hubOracleOneShotOutputIndex,
    OPERATOR_REQUIRED_BOND_LOVELACE: operatorRequiredBondLovelace,
    OPERATOR_SLASHING_PENALTY_LOVELACE: operatorSlashingPenaltyLovelace,
    DA_COMMITTEE_HEX: daCommitteeHex,
    DA_THRESHOLD: daThreshold,
    WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES: waitBetweenDepositUTxOFetches,
    PROM_METRICS_PORT: promMetricsPort,
    OLTP_EXPORTER_URL: oltpExporterUrl,
    POSTGRES_HOST: postgresHost,
    POSTGRES_PORT: postgresPort,
    POSTGRES_PASSWORD: postgresPassword,
    POSTGRES_DB: postgresDb,
    POSTGRES_USER: postgresUser,
    LEDGER_MPF_DB_PATH: ledgerMpfDbPath,
    TRANSACTIONS_MPF_DB_PATH: transactionsMpfDbPath,
    GENESIS_UTXOS: network === "Mainnet" ? [] : genesisUtxos,
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
