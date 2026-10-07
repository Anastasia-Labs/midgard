import { createHash } from "node:crypto";
import { availableParallelism } from "node:os";

import {
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
} from "@al-ft/midgard-core/cek-proof";
import { type DeploymentManifestEconomicsProfile } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type L1Origin } from "@al-ft/midgard-core/l1-origin";
import { Network, UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Config } from "effect";

import {
  positiveFiniteNumber,
  positiveSafeInteger,
} from "../artifact-schema.js";
import { isStrictlyAscending, splitPackedHex } from "../da/local-signers.js";
import { type NativeLedgerSettings } from "./native-ledger.js";

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
export const validateDaKeySetEncoding = (
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

export const boundedValidationInteger = (
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
  /** Local ledger for reward-account reads; Ogmios cannot answer them. */
  L1_NATIVE_LEDGER: NativeLedgerSettings | undefined;
  /** Operator-approved lossless Shelley query result hash; required by listen. */
  L1_HISTORY_GENESIS_LOSSLESS_SHA256: string;
  L1_OPERATOR_SEED_PHRASE: string;
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  /** Required by listen; optional for read-only and deployment commands. */
  L1_SETTLEMENT_SEED_PHRASE?: string;
  L1_REFERENCE_SCRIPT_SEED_PHRASE: string;
  L1_REFERENCE_SCRIPT_ADDRESS: string;
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: string;
  REFERENCE_SCRIPT_AUTH_TIMELOCK_MS: number;
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS: number;
  NETWORK: Network;
  DEPLOYMENT_ECONOMICS_PROFILE: DeploymentManifestEconomicsProfile;
  PORT: number;
  WAIT_BETWEEN_BLOCK_COMMITMENT: number;
  WAIT_BETWEEN_BLOCK_CONFIRMATION: number;
  SPECULATIVE_COMMIT_BUILD: boolean;
  OPERATOR_WATCHDOG_ENABLED: boolean;
  OPERATOR_WATCHDOG_PATIENCE_MS: number;
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
  /** Explicit housekeeping window in days; undefined when unset, which means
   * the verified deployment manifest's window (`resolveHousekeepingRetentionDays`). */
  RETENTION_DAYS: number | undefined;
  WAIT_BETWEEN_RETENTION_SWEEPS: number;
  L1_VIEW_FATAL_MS: number;
  HUB_ORACLE_ONE_SHOT_TX_HASH: string;
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  /** The operator-configured L1 origin point; null when `L1_ORIGIN` is unset. */
  L1_ORIGIN: L1Origin | null;
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
  MPF_SCRATCH_BUILD: "insert" | "fromlist";
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

export const positiveSafeIntegerConfig = (name: string, defaultValue: number) =>
  Config.integer(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => positiveSafeInteger(value, name)),
  );

export const positiveFiniteNumberConfig = (
  name: string,
  defaultValue: number,
) =>
  Config.number(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => positiveFiniteNumber(value, name)),
  );

/**
 * Reads a wallet seed phrase that the node cannot start without. Fails config
 * load with the variable named and the fix stated, instead of letting the
 * wallet derivation surface a bare bip39 "invalid mnemonic" defect for an
 * empty `.env` value.
 */
export const requiredSeedPhrase = (name: string) =>
  Config.string(name).pipe(
    Config.mapAttempt((value) => validateSeedPhrase(name, value)),
  );

/**
 * Wallet derivation runs bip39 PBKDF2 in wasm, tens of milliseconds a call,
 * and every NodeConfig load derives up to four wallets. Hosts that rebuild
 * the config layer per effect (commands, tests) paid that on every run. The
 * derived address is a pure function of (network, phrase), so it is memoized
 * under a SHA-256 of that pair: the cache holds no seed material, only public
 * addresses. A failed derivation is not cached and throws again every load.
 */
const seedAddresses = new Map<string, string>();
const SEED_ADDRESS_CACHE_LIMIT = 64;

export const seedPhraseAddress = (
  seedPhrase: string,
  network: Network,
): string => {
  const key = createHash("sha256")
    .update(network)
    .update("\0")
    .update(seedPhrase)
    .digest("hex");
  const cached = seedAddresses.get(key);
  if (cached !== undefined) return cached;
  const { address } = walletFromSeed(seedPhrase, { network });
  if (seedAddresses.size >= SEED_ADDRESS_CACHE_LIMIT) seedAddresses.clear();
  seedAddresses.set(key, address);
  return address;
};

export const validateSeedPhrase = (name: string, value: string): string => {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    throw new Error(
      `${name} is required: set a wallet seed phrase in .env (see the "Required settings" block at the top of .env.example).`,
    );
  }
  try {
    // Address prefix depends on the network, mnemonic validity does not, so a
    // fixed network is enough to reject a malformed phrase at config load.
    seedPhraseAddress(trimmed, "Preprod");
  } catch (cause) {
    throw new Error(
      `${name} is not a valid wallet seed phrase: ${String(cause)}`,
    );
  }
  return trimmed;
};
