import { createHash, randomBytes } from "node:crypto";
import {
  access,
  chmod,
  lstat,
  mkdir,
  open,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { dirname, join } from "node:path";

import {
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardLedgerTxFromCanonicalCbor } from "@al-ft/midgard-validation/ledger-tx/codec";
import { CML, type Network } from "@lucid-evolution/lucid";
import { generateMnemonic } from "bip39";
import { Effect } from "effect";

import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
} from "@/commands/command-utils.js";
import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "@/files/atomic-write.js";

export const STRESS_WALLET_SCHEMA_VERSION = "midgard-stress-wallet-v1";
export const STRESS_WALLET_PREPARE_SCHEMA_VERSION =
  "midgard-stress-wallet-prepare-v1";
export const STRESS_WALLET_FANOUT_SCHEMA_VERSION =
  "midgard-stress-wallet-fanout-v1";
export const STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION =
  "midgard-stress-wallet-consolidate-v2";
export const STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION =
  "midgard-stress-wallet-terminal-drain-v1";
export const DEFAULT_STRESS_WALLET_DIR = ".stress-wallets";
export const DEFAULT_STRESS_WALLET_ENV_PREFIX = "STRESS_WALLET_SEED_PHRASE";
export const DEFAULT_PROJECTION_WAIT_MS = 120_000;
export const DEFAULT_VERIFY_TIMEOUT_MS = 300_000;
export const DEFAULT_VERIFY_POLL_INTERVAL_MS = 5_000;
export const DEFAULT_FANOUT_BRANCH_FACTOR = 16;
export const DEFAULT_FANOUT_MAX_IN_FLIGHT = 32;
export const DEFAULT_FANOUT_FEE_HEADROOM_LOVELACE = 500_000n;
export const DEFAULT_CONSOLIDATE_RESERVE_LOVELACE = 100_000n;
export const DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT = 32;
const FANOUT_UTXO_QUERY_MAX_ATTEMPTS = 6;
const FANOUT_UTXO_QUERY_INITIAL_RETRY_MS = 250;
const FANOUT_UTXO_QUERY_MAX_RETRY_MS = 5_000;

const ENV_NAME_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

export const runWithSharedFanoutContext = <A, R>(
  run: (
    execute: <B, E>(effect: Effect.Effect<B, E, R>) => Promise<B>,
  ) => Promise<A>,
): Effect.Effect<A, never, R> =>
  Effect.gen(function* () {
    const context = yield* Effect.context<R>();
    return yield* Effect.promise(() =>
      run((effect) => Effect.runPromise(Effect.provide(effect, context))),
    );
  });

export type StressWalletFundingSnapshot = {
  readonly preparedAt: string;
  readonly status: "submitted" | "already_funded";
  readonly lovelacePerWallet: string;
  readonly nodeEndpoint: string;
  readonly beforeUtxoCount: number;
  readonly afterUtxoCount: number;
  readonly verifiedFundingUtxoCount: number;
  readonly fundingUtxos?: readonly StressWalletFundingUtxoSnapshot[];
  readonly depositTxHash?: string;
  readonly depositEventId?: string;
};

export type StressWalletFundingUtxoSnapshot = {
  readonly outref: string;
  readonly outputCbor: string;
  readonly lovelace: string;
};

export type StressWalletRecord = {
  readonly schemaVersion: typeof STRESS_WALLET_SCHEMA_VERSION;
  readonly walletId: string;
  readonly index: number;
  readonly envName: string;
  readonly network: Network;
  readonly seedPhrase: string;
  readonly l2Address: string;
  readonly paymentKeyHash: string;
  readonly createdAt: string;
  readonly latestFunding?: StressWalletFundingSnapshot;
};

export type StressWalletSummary = Omit<StressWalletRecord, "seedPhrase"> & {
  readonly path: string;
};

export type StressWalletExportArtifacts = {
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly envNames: readonly string[];
};

export type CreateL2WalletsOptions = {
  readonly count: number;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly overwrite?: boolean;
  readonly reuseExisting?: boolean;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type CreateL2WalletsResult = {
  readonly schemaVersion: typeof STRESS_WALLET_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly createdCount: number;
  readonly reusedCount: number;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly wallets: readonly StressWalletSummary[];
};

export type StressWalletDepositRequest = {
  readonly wallet: StressWalletRecord;
  readonly lovelace: bigint;
};

export type StressWalletDepositResult = {
  readonly txHash: string;
  readonly depositEventId?: string;
};

export type PrepareStressWalletsRuntime = {
  readonly submitDeposit: (
    request: StressWalletDepositRequest,
  ) => Promise<StressWalletDepositResult>;
  readonly projectDeposits: () => Promise<void>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type PrepareStressWalletsOptions = {
  readonly count: number;
  readonly lovelacePerWallet: bigint;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly createMissing?: boolean;
  readonly forceFundExisting?: boolean;
  readonly projectionWaitMs?: number;
  readonly verifyTimeoutMs?: number;
  readonly pollIntervalMs?: number;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type StressWalletPrepareEntry = {
  readonly wallet: StressWalletSummary;
  readonly status: "submitted" | "already_funded";
  readonly beforeUtxoCount: number;
  readonly afterUtxoCount: number;
  readonly verifiedFundingUtxoCount: number;
  readonly depositTxHash?: string;
  readonly depositEventId?: string;
};

export type PrepareStressWalletsResult = {
  readonly schemaVersion: typeof STRESS_WALLET_PREPARE_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly generatedWalletCount: number;
  readonly submittedDepositCount: number;
  readonly alreadyFundedCount: number;
  readonly verifiedWalletCount: number;
  readonly lovelacePerWallet: string;
  readonly nodeEndpoint: string;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly wallets: readonly StressWalletPrepareEntry[];
};

export type StressWalletFanoutSource =
  | {
      readonly kind: "treasury";
      readonly seedPhrase: string;
      readonly walletId: "treasury";
    }
  | {
      readonly kind: "wallet";
      readonly wallet: StressWalletRecord;
    };

export type StressWalletFanoutTransferRequest = {
  readonly source: StressWalletFanoutSource;
  readonly destination: StressWalletRecord;
  readonly lovelace: bigint;
  readonly level: number;
};

export type StressWalletFanoutTransferResult = {
  readonly txHash: string;
  readonly status?: string;
};

export type StressWalletFanoutRuntime = {
  readonly submitTransfer: (
    request: StressWalletFanoutTransferRequest,
  ) => Promise<StressWalletFanoutTransferResult>;
  readonly fetchTxStatus: (
    nodeEndpoint: string,
    txHash: string,
  ) => Promise<string>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type FanoutStressWalletsOptions = {
  readonly count: number;
  readonly lovelacePerWallet: bigint;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly createMissing?: boolean;
  readonly branchFactor?: number;
  readonly maxInFlight?: number;
  readonly feeHeadroomLovelace?: bigint;
  readonly acceptanceTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number;
  readonly pollMaxIntervalMs?: number;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type StressWalletFanoutEdgeSummary = {
  readonly level: number;
  readonly parentWalletId: "treasury" | string;
  readonly childWalletId: string;
  readonly lovelace: string;
  readonly txHash: string;
  readonly acceptedStatus: string;
  readonly submitted: boolean;
};

export type StressWalletFanoutEntry = {
  readonly wallet: StressWalletSummary;
  readonly verifiedFundingUtxoCount: number;
};

export type StressWalletFanoutResult = {
  readonly schemaVersion: typeof STRESS_WALLET_FANOUT_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly generatedWalletCount: number;
  readonly branchFactor: number;
  readonly maxInFlight: number;
  readonly lovelacePerWallet: string;
  readonly feeHeadroomLovelace: string;
  readonly rootRequiredLovelace: string;
  readonly submittedTransferCount: number;
  readonly alreadyFundedTransferCount: number;
  readonly verifiedWalletCount: number;
  readonly nodeEndpoint: string;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly reportPath: string;
  readonly levels: readonly {
    readonly level: number;
    readonly transferCount: number;
  }[];
  readonly wallets: readonly StressWalletFanoutEntry[];
};

export type StressWalletConsolidateTransferRequest = {
  readonly source: StressWalletRecord;
  readonly treasuryAddress: string;
  readonly lovelace: bigint;
};

export type StressWalletPreparedConsolidateTransfer = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly selectedInputs: readonly string[];
};

export type StressWalletSubmittedConsolidateTransfer = {
  readonly txHash: string;
  readonly status: string;
};

export type StressWalletConsolidationReadinessResponse = {
  readonly httpStatus: number;
  readonly body: unknown;
};

export type StressWalletConsolidateRuntime = {
  readonly prepareTransfer: (
    request: StressWalletConsolidateTransferRequest,
  ) => Promise<StressWalletPreparedConsolidateTransfer>;
  readonly submitPreparedTransfer: (request: {
    readonly nodeEndpoint: string;
    readonly txHash: string;
    readonly signedTxCbor: string;
  }) => Promise<StressWalletSubmittedConsolidateTransfer>;
  readonly fetchTxStatus: (
    nodeEndpoint: string,
    txHash: string,
  ) => Promise<string>;
  readonly fetchReadiness?: (
    nodeEndpoint: string,
  ) => Promise<StressWalletConsolidationReadinessResponse>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type ConsolidateStressWalletsOptions = {
  readonly count: number;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly reserveLovelace?: bigint;
  readonly requiredTreasuryLovelace?: bigint;
  readonly maxInFlight?: number;
  readonly acceptanceTimeoutMs?: number;
  readonly readinessTimeoutMs?: number;
  readonly verificationTimeoutMs?: number;
  readonly requestTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number;
  readonly pollMaxIntervalMs?: number;
  readonly now?: () => Date;
};

export type StressWalletConsolidateResult = {
  readonly schemaVersion: typeof STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly reserveLovelace: string;
  readonly maxInFlight: number;
  readonly nodeEndpoint: string;
  readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string;
  readonly treasuryAfterLovelace: string;
  readonly treasuryDeltaLovelace: string;
  readonly sourceBeforeLovelace: string;
  readonly sourceAfterLovelace: string;
  readonly inferredFeesLovelace: string;
  readonly projectedTreasuryLovelace: string;
  readonly submittedTransferCount: number;
  readonly resumedTransferCount: number;
  readonly alreadyConsolidatedCount: number;
  readonly reportPath: string;
};

export type StressWalletPreparedTerminalDrain = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly selectedInputs: readonly string[];
  readonly requestedLovelace: bigint;
  readonly feeLovelace: bigint;
  readonly signedTxBytes: number;
};

export type StressWalletTerminalDrainRuntime = {
  readonly prepareTransfer: (request: { readonly source: StressWalletRecord; readonly treasuryAddress: string }) => Promise<StressWalletPreparedTerminalDrain>;
  readonly submitPreparedTransfer: StressWalletConsolidateRuntime["submitPreparedTransfer"];
  readonly fetchTxStatus: StressWalletConsolidateRuntime["fetchTxStatus"];
  readonly fetchUtxos?: StressWalletConsolidateRuntime["fetchUtxos"];
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type TerminalDrainStressWalletsOptions = {
  readonly count: number; readonly treasurySeedPhrase: string; readonly nodeEndpoint?: string;
  readonly outDir?: string; readonly startIndex?: number; readonly envPrefix?: string; readonly network?: Network;
  readonly minFeeA: bigint; readonly minFeeB: bigint; readonly feeCapLovelace?: bigint;
  readonly maxFeeIterations?: number; readonly maxInFlight?: number; readonly prepareOnly?: boolean;
  readonly acceptanceTimeoutMs?: number; readonly verificationTimeoutMs?: number; readonly requestTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number; readonly pollMaxIntervalMs?: number; readonly now?: () => Date;
};

export type StressWalletTerminalDrainResult = {
  readonly schemaVersion: typeof STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION;
  readonly phase: "prepared" | "committed"; readonly walletDirectory: string; readonly requestedCount: number;
  readonly nodeEndpoint: string; readonly treasuryAddress: string; readonly treasuryBeforeLovelace: string;
  readonly treasuryAfterLovelace?: string; readonly treasuryDeltaLovelace?: string; readonly grossSourceLovelace: string;
  readonly totalFeesLovelace: string; readonly residualSourceLovelace?: string; readonly preparedTransferCount: number;
  readonly alreadyEmptyCount: number; readonly submittedTransferCount: number; readonly resumedTransferCount: number;
  readonly statePath: string; readonly reportPath?: string;
};

type ResolvedStressWallet = {
  readonly path: string;
  readonly record: StressWalletRecord;
  readonly created: boolean;
};

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

const fileExists = async (path: string): Promise<boolean> =>
  access(path).then(
    () => true,
    () => false,
  );

const sha256Bytes = (bytes: Buffer): string =>
  createHash("sha256").update(bytes).digest("hex");

const readFileGeneration = async (
  path: string,
): Promise<string | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return sha256Bytes(await readFile(path));
};

const assertFileGeneration = async (
  path: string,
  expectedSha256: string | undefined,
): Promise<void> => {
  const observedSha256 = await readFileGeneration(path);
  if (observedSha256 !== expectedSha256) {
    throw new Error(
      `Consolidation state generation changed at ${path}; expected ${expectedSha256 ?? "<absent>"}, observed ${observedSha256 ?? "<absent>"}. Refusing to overwrite concurrent or manual changes.`,
    );
  }
};

const withExclusiveConsolidationStateLock = async <A>(
  statePath: string,
  action: () => Promise<A>,
): Promise<A> => {
  const lockPath = `${statePath}.lock`;
  const lockToken = randomBytes(32).toString("hex");
  await mkdir(dirname(statePath), { recursive: true, mode: 0o700 });
  let handle: Awaited<ReturnType<typeof open>>;
  try {
    handle = await open(lockPath, "wx", 0o600);
  } catch (cause) {
    throw new Error(
      `Consolidation state is exclusively locked at ${lockPath}. A stale lock must be archived or removed only after proving its recorded process is not running.`,
      { cause },
    );
  }
  const heldIdentity = await handle.stat();
  try {
    await handle.writeFile(
      `${formatJson({
        token: lockToken,
        pid: process.pid,
        acquiredAt: new Date().toISOString(),
      })}\n`,
      "utf8",
    );
    await handle.sync();
    return await action();
  } finally {
    let ownsPublishedLock = false;
    try {
      const publishedIdentity = await lstat(lockPath);
      const published = JSON.parse(await readFile(lockPath, "utf8")) as {
        readonly token?: unknown;
      };
      ownsPublishedLock =
        publishedIdentity.dev === heldIdentity.dev &&
        publishedIdentity.ino === heldIdentity.ino &&
        published.token === lockToken;
    } catch {
      ownsPublishedLock = false;
    }
    await handle.close();
    if (!ownsPublishedLock) {
      throw new Error(
        `Consolidation lock ownership changed at ${lockPath}; refusing to remove a missing or replacement lock.`,
      );
    }
    await rm(lockPath);
  }
};

const withExclusiveStressWalletFundsLock = async <A>(
  outDir: string,
  action: () => Promise<A>,
): Promise<A> => {
  const lockPath = join(outDir, "stress-wallet-funds.lock");
  const lockToken = randomBytes(32).toString("hex");
  await mkdir(outDir, { recursive: true, mode: 0o700 });
  let handle: Awaited<ReturnType<typeof open>>;
  try {
    handle = await open(lockPath, "wx", 0o600);
  } catch (cause) {
    throw new Error(
      "Stress-wallet funds operations are exclusively locked at " + lockPath + ". A stale lock must be archived or removed only after proving its recorded process is not running.",
      { cause },
    );
  }
  const heldIdentity = await handle.stat();
  try {
    await handle.writeFile(formatJson({ token: lockToken, pid: process.pid, acquiredAt: new Date().toISOString() }) + "\n", "utf8");
    await handle.sync();
    return await action();
  } finally {
    let ownsPublishedLock = false;
    try {
      const publishedIdentity = await lstat(lockPath);
      const published = JSON.parse(await readFile(lockPath, "utf8")) as { readonly token?: unknown };
      ownsPublishedLock = publishedIdentity.dev === heldIdentity.dev && publishedIdentity.ino === heldIdentity.ino && published.token === lockToken;
    } catch {
      ownsPublishedLock = false;
    }
    await handle.close();
    if (!ownsPublishedLock) throw new Error("Stress-wallet funds lock ownership changed at " + lockPath + "; refusing to remove a missing or replacement lock.");
    await rm(lockPath);
  }
};

const writePrivateFileAtomic = async (
  path: string,
  contents: string,
): Promise<void> => writeTextFileAtomic(path, contents, { mode: 0o600 });

const writePrivateFileAtomicNoReplace = async (
  path: string,
  contents: string,
): Promise<void> =>
  writeTextFileAtomicNoReplace(path, contents, { mode: 0o600 });

const requireSafePositiveInteger = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a safe positive integer.`);
  }
  return value;
};

const requireSafeNonNegativeInteger = (
  value: number,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} must be a safe non-negative integer.`);
  }
  return value;
};

export const parseStressWalletNetwork = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): Network => {
  const normalized = (value ?? env.NETWORK ?? "Preprod").trim();
  if (
    normalized === "Mainnet" ||
    normalized === "Preprod" ||
    normalized === "Preview"
  ) {
    return normalized;
  }
  throw new Error(
    `Unsupported network "${normalized}". Expected Mainnet, Preprod, or Preview.`,
  );
};

export const parseStressWalletCount = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = Number(value);
  return requireSafePositiveInteger(parsed, label);
};

export const parseStressWalletNonNegativeMs = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  const parsed = Number(value);
  return requireSafeNonNegativeInteger(parsed, label);
};

export const parseStressWalletLovelace = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = BigInt(value);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

export const parseStressWalletNonNegativeLovelace = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(value);
};

const normalizeEnvPrefix = (value: string | undefined): string => {
  const prefix = value?.trim() || DEFAULT_STRESS_WALLET_ENV_PREFIX;
  if (!ENV_NAME_PATTERN.test(`${prefix}_0001`)) {
    throw new Error(
      `Stress wallet env prefix "${prefix}" does not produce valid environment variable names.`,
    );
  }
  return prefix;
};

const walletIndexLabel = (index: number): string =>
  index.toString().padStart(4, "0");

export const stressWalletFileName = (index: number): string =>
  `wallet-${walletIndexLabel(index)}.json`;

export const stressWalletEnvName = (envPrefix: string, index: number): string =>
  `${envPrefix}_${walletIndexLabel(index)}`;

const walletPath = (outDir: string, index: number): string =>
  join(outDir, stressWalletFileName(index));

const defaultGenerateSeedPhrase = (): string => generateMnemonic(256);

const normalizeSeedPhrase = (seedPhrase: string): string =>
  seedPhrase.trim().replace(/\s+/g, " ");

const stressWalletId = (index: number): string =>
  "stress-wallet-" + walletIndexLabel(index);

const deriveStressWalletRecord = ({
  index,
  envName,
  network,
  seedPhrase,
  now,
}: {
  readonly index: number;
  readonly envName: string;
  readonly network: Network;
  readonly seedPhrase: string;
  readonly now: () => Date;
}): StressWalletRecord => {
  const normalizedSeed = normalizeSeedPhrase(seedPhrase);
  const walletInfo = deriveWalletInfo(
    { seedPhrase: normalizedSeed, resolvedFrom: envName },
    network,
  );
  return {
    schemaVersion: STRESS_WALLET_SCHEMA_VERSION,
    walletId: `stress-wallet-${walletIndexLabel(index)}`,
    index,
    envName,
    network,
    seedPhrase: normalizedSeed,
    l2Address: walletInfo.address,
    paymentKeyHash: walletInfo.paymentKeyHash,
    createdAt: now().toISOString(),
  };
};

const asObject = (
  value: unknown,
  fieldName: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${fieldName} must be an object.`);
  }
  return value as Record<string, unknown>;
};

const requiredString = (value: unknown, fieldName: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${fieldName} must be a non-empty string.`);
  }
  return value.trim();
};

const requiredPositiveInteger = (value: unknown, fieldName: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${fieldName} must be a safe positive integer.`);
  }
  return value;
};

const requiredPositiveIntegerOrZero = (
  value: unknown,
  fieldName: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${fieldName} must be a safe non-negative integer.`);
  }
  return value;
};

const parseLatestFunding = (
  value: unknown,
): StressWalletFundingSnapshot | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const raw = asObject(value, "latestFunding");
  const status = requiredString(raw.status, "latestFunding.status");
  if (status !== "submitted" && status !== "already_funded") {
    throw new Error(
      "latestFunding.status must be submitted or already_funded.",
    );
  }
  const snapshot: StressWalletFundingSnapshot = {
    preparedAt: requiredString(raw.preparedAt, "latestFunding.preparedAt"),
    status,
    lovelacePerWallet: requiredString(
      raw.lovelacePerWallet,
      "latestFunding.lovelacePerWallet",
    ),
    nodeEndpoint: requiredString(
      raw.nodeEndpoint,
      "latestFunding.nodeEndpoint",
    ),
    beforeUtxoCount: requiredPositiveIntegerOrZero(
      raw.beforeUtxoCount,
      "latestFunding.beforeUtxoCount",
    ),
    afterUtxoCount: requiredPositiveIntegerOrZero(
      raw.afterUtxoCount,
      "latestFunding.afterUtxoCount",
    ),
    verifiedFundingUtxoCount: requiredPositiveIntegerOrZero(
      raw.verifiedFundingUtxoCount,
      "latestFunding.verifiedFundingUtxoCount",
    ),
    ...(raw.fundingUtxos === undefined
      ? {}
      : {
          fundingUtxos: parseFundingUtxoSnapshots(raw.fundingUtxos),
        }),
    ...(raw.depositTxHash === undefined
      ? {}
      : { depositTxHash: requiredString(raw.depositTxHash, "depositTxHash") }),
    ...(raw.depositEventId === undefined
      ? {}
      : {
          depositEventId: requiredString(raw.depositEventId, "depositEventId"),
        }),
  };
  return snapshot;
};

const parseFundingUtxoSnapshots = (
  value: unknown,
): readonly StressWalletFundingUtxoSnapshot[] => {
  if (!Array.isArray(value)) {
    throw new Error("latestFunding.fundingUtxos must be an array.");
  }
  return value.map((entry, index) => {
    const raw = asObject(
      entry,
      `latestFunding.fundingUtxos[${index.toString()}]`,
    );
    return {
      outref: requiredString(
        raw.outref,
        `latestFunding.fundingUtxos[${index.toString()}].outref`,
      ),
      outputCbor: requiredString(
        raw.outputCbor,
        `latestFunding.fundingUtxos[${index.toString()}].outputCbor`,
      ),
      lovelace: requiredString(
        raw.lovelace,
        `latestFunding.fundingUtxos[${index.toString()}].lovelace`,
      ),
    };
  });
};

export const parseStressWalletRecord = (value: unknown): StressWalletRecord => {
  const raw = asObject(value, "stress wallet record");
  const schemaVersion = requiredString(raw.schemaVersion, "schemaVersion");
  if (schemaVersion !== STRESS_WALLET_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress wallet schemaVersion "${schemaVersion}".`,
    );
  }
  const network = parseStressWalletNetwork(
    requiredString(raw.network, "network"),
    {},
  );
  const record: StressWalletRecord = {
    schemaVersion: STRESS_WALLET_SCHEMA_VERSION,
    walletId: requiredString(raw.walletId, "walletId"),
    index: requiredPositiveInteger(raw.index, "index"),
    envName: requiredString(raw.envName, "envName"),
    network,
    seedPhrase: normalizeSeedPhrase(
      requiredString(raw.seedPhrase, "seedPhrase"),
    ),
    l2Address: requiredString(raw.l2Address, "l2Address"),
    paymentKeyHash: requiredString(raw.paymentKeyHash, "paymentKeyHash"),
    createdAt: requiredString(raw.createdAt, "createdAt"),
    latestFunding: parseLatestFunding(raw.latestFunding),
  };
  const derived = deriveStressWalletRecord({
    index: record.index,
    envName: record.envName,
    network: record.network,
    seedPhrase: record.seedPhrase,
    now: () => new Date(record.createdAt),
  });
  if (derived.l2Address !== record.l2Address) {
    throw new Error(
      `Stress wallet ${record.walletId} seed phrase derives ${derived.l2Address}, not recorded address ${record.l2Address}.`,
    );
  }
  if (derived.paymentKeyHash !== record.paymentKeyHash) {
    throw new Error(
      `Stress wallet ${record.walletId} seed phrase derives a different payment key hash.`,
    );
  }
  return record;
};

const validateExistingWalletRecord = ({
  record,
  path,
  expectedIndex,
  expectedWalletId,
  expectedEnvName,
  expectedNetwork,
}: {
  readonly record: StressWalletRecord;
  readonly path: string;
  readonly expectedIndex: number;
  readonly expectedWalletId: string;
  readonly expectedEnvName: string;
  readonly expectedNetwork: Network;
}): void => {
  if (record.walletId !== expectedWalletId) {
    throw new Error(
      `Stress wallet file ${path} records walletId ${record.walletId}, expected ${expectedWalletId}.`,
    );
  }
  if (record.index !== expectedIndex) {
    throw new Error(
      `Stress wallet file ${path} records index ${record.index.toString()}, expected ${expectedIndex.toString()}.`,
    );
  }
  if (record.envName !== expectedEnvName) {
    throw new Error(
      `Stress wallet file ${path} records envName ${record.envName}, expected ${expectedEnvName}.`,
    );
  }
  if (record.network !== expectedNetwork) {
    throw new Error(
      `Stress wallet file ${path} records network ${record.network}, expected ${expectedNetwork}.`,
    );
  }
};

const readStressWalletRecord = async (
  path: string,
): Promise<StressWalletRecord> =>
  parseStressWalletRecord(JSON.parse(await readFile(path, "utf8")) as unknown);

const writeStressWalletRecord = async (
  path: string,
  record: StressWalletRecord,
): Promise<void> => {
  await writePrivateFileAtomic(path, `${formatJson(record)}\n`);
};

const summaryForRecord = (
  record: StressWalletRecord,
  path: string,
): StressWalletSummary => ({
  schemaVersion: record.schemaVersion,
  walletId: record.walletId,
  index: record.index,
  envName: record.envName,
  network: record.network,
  l2Address: record.l2Address,
  paymentKeyHash: record.paymentKeyHash,
  createdAt: record.createdAt,
  latestFunding: record.latestFunding,
  path,
});

const shellSingleQuote = (value: string): string =>
  `'${value.replaceAll("'", "'\\''")}'`;

const writeStressWalletExports = async (
  outDir: string,
  records: readonly StressWalletRecord[],
): Promise<StressWalletExportArtifacts> => {
  const envFilePath = join(outDir, "stress-wallets.env");
  const argsFilePath = join(outDir, "stress-wallets.args");
  const envFile = [
    "# Generated by midgard-node create-l2-wallet/stress-wallets:prepare.",
    "# Contains private seed phrases; keep this file local.",
    ...records.map(
      (record) =>
        `export ${record.envName}=${shellSingleQuote(record.seedPhrase)}`,
    ),
    "",
  ].join("\n");
  const argsLines = records.map(
    (record) => `--stress-wallet-seed-phrase-env ${record.envName}`,
  );
  await writePrivateFileAtomic(envFilePath, envFile);
  await writeFile(argsFilePath, `${argsLines.join("\n")}\n`, "utf8");
  return {
    envFilePath,
    argsFilePath,
    envNames: records.map((record) => record.envName),
  };
};

const validateDistinctWallets = (
  records: readonly StressWalletRecord[],
): void => {
  const walletIds = new Set<string>();
  const envNames = new Set<string>();
  const addresses = new Set<string>();
  const paymentKeyHashes = new Set<string>();
  for (const record of records) {
    if (walletIds.has(record.walletId)) {
      throw new Error("Duplicate stress walletId " + record.walletId + ".");
    }
    if (paymentKeyHashes.has(record.paymentKeyHash)) {
      throw new Error(
        "Duplicate stress wallet payment key hash " +
          record.paymentKeyHash +
          ".",
      );
    }
    if (!ENV_NAME_PATTERN.test(record.envName)) {
      throw new Error(`Invalid stress wallet env name "${record.envName}".`);
    }
    if (envNames.has(record.envName)) {
      throw new Error(`Duplicate stress wallet env name "${record.envName}".`);
    }
    if (addresses.has(record.l2Address)) {
      throw new Error(
        `Duplicate stress wallet L2 address ${record.l2Address}.`,
      );
    }
    walletIds.add(record.walletId);
    envNames.add(record.envName);
    addresses.add(record.l2Address);
    paymentKeyHashes.add(record.paymentKeyHash);
  }
};

const resolveStressWalletRecords = async ({
  count,
  outDir,
  startIndex,
  envPrefix,
  network,
  overwrite,
  reuseExisting,
  createMissing,
  now,
  generateSeedPhrase,
}: {
  readonly count: number;
  readonly outDir: string;
  readonly startIndex: number;
  readonly envPrefix: string;
  readonly network: Network;
  readonly overwrite: boolean;
  readonly reuseExisting: boolean;
  readonly createMissing: boolean;
  readonly now: () => Date;
  readonly generateSeedPhrase: () => string;
}): Promise<readonly ResolvedStressWallet[]> => {
  await mkdir(outDir, { recursive: true, mode: 0o700 });
  const resolved: ResolvedStressWallet[] = [];
  for (let offset = 0; offset < count; offset += 1) {
    const index = startIndex + offset;
    const path = walletPath(outDir, index);
    const expectedEnvName = stressWalletEnvName(envPrefix, index);
    const exists = await fileExists(path);
    if (exists && !overwrite) {
      if (!reuseExisting) {
        throw new Error(
          `Stress wallet file already exists at ${path}; pass --reuse-existing or --overwrite to proceed.`,
        );
      }
      const record = await readStressWalletRecord(path);
      validateExistingWalletRecord({
        record,
        path,
        expectedIndex: index,
        expectedWalletId: stressWalletId(index),
        expectedEnvName,
        expectedNetwork: network,
      });
      resolved.push({
        path,
        record,
        created: false,
      });
      continue;
    }
    if (!exists && !createMissing) {
      throw new Error(
        `Missing stress wallet file at ${path}; run create-l2-wallet first or pass --create-missing.`,
      );
    }
    const record = deriveStressWalletRecord({
      index,
      envName: expectedEnvName,
      network,
      seedPhrase: generateSeedPhrase(),
      now,
    });
    await writeStressWalletRecord(path, record);
    resolved.push({ path, record, created: true });
  }
  validateDistinctWallets(resolved.map(({ record }) => record));
  return resolved;
};

export const createL2Wallets = async (
  options: CreateL2WalletsOptions,
): Promise<CreateL2WalletsResult> => {
  const count = options.count;
  requireSafePositiveInteger(count, "count");
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const resolved = await resolveStressWalletRecords({
    count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: options.overwrite === true,
    reuseExisting: options.reuseExisting === true,
    createMissing: true,
    now: options.now ?? (() => new Date()),
    generateSeedPhrase: options.generateSeedPhrase ?? defaultGenerateSeedPhrase,
  });
  const exports = await writeStressWalletExports(
    outDir,
    resolved.map(({ record }) => record),
  );
  return {
    schemaVersion: STRESS_WALLET_SCHEMA_VERSION,
    walletDirectory: outDir,
    createdCount: resolved.filter((wallet) => wallet.created).length,
    reusedCount: resolved.filter((wallet) => !wallet.created).length,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    wallets: resolved.map(({ path, record }) => summaryForRecord(record, path)),
  };
};

const outRefKey = (utxo: NodeUtxo): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const fundingUtxos = (
  utxos: readonly NodeUtxo[],
  lovelacePerWallet: bigint,
): readonly NodeUtxo[] =>
  utxos.filter((utxo) => (utxo.assets.lovelace ?? 0n) >= lovelacePerWallet);

const newFundingUtxos = ({
  before,
  after,
  lovelacePerWallet,
}: {
  readonly before: readonly NodeUtxo[];
  readonly after: readonly NodeUtxo[];
  readonly lovelacePerWallet: bigint;
}): readonly NodeUtxo[] => {
  const beforeOutRefs = new Set(before.map(outRefKey));
  return fundingUtxos(after, lovelacePerWallet).filter(
    (utxo) => !beforeOutRefs.has(outRefKey(utxo)),
  );
};

const queryWalletUtxos = async ({
  records,
  nodeEndpoint,
  fetchUtxos,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly nodeEndpoint: string;
  readonly fetchUtxos: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
}): Promise<Map<string, readonly NodeUtxo[]>> => {
  const entries = await Promise.all(
    records.map(
      async (record) =>
        [
          record.envName,
          await fetchUtxos(nodeEndpoint, record.l2Address),
        ] as const,
    ),
  );
  return new Map(entries);
};

const prepareStressWalletsUnlocked = async (
  options: PrepareStressWalletsOptions,
  runtime: PrepareStressWalletsRuntime,
): Promise<PrepareStressWalletsResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.lovelacePerWallet <= 0n) {
    throw new Error("lovelacePerWallet must be greater than zero.");
  }
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const now = runtime.now ?? options.now ?? (() => new Date());
  const nodeEndpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const fetchUtxos = runtime.fetchUtxos ?? fetchNodeUtxosByAddress;
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const projectionWaitMs = requireSafeNonNegativeInteger(
    options.projectionWaitMs ?? DEFAULT_PROJECTION_WAIT_MS,
    "projectionWaitMs",
  );
  const verifyTimeoutMs = requireSafeNonNegativeInteger(
    options.verifyTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "verifyTimeoutMs",
  );
  const pollIntervalMs = requireSafeNonNegativeInteger(
    options.pollIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS,
    "pollIntervalMs",
  );
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: options.createMissing === true,
    now,
    generateSeedPhrase: options.generateSeedPhrase ?? defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const pathsByEnv = new Map(
    resolved.map(({ path, record }) => [record.envName, path] as const),
  );
  const exports = await writeStressWalletExports(outDir, records);
  const beforeUtxos = await queryWalletUtxos({
    records,
    nodeEndpoint,
    fetchUtxos,
  });

  const pendingEntries: Array<{
    readonly record: StressWalletRecord;
    readonly status: "submitted" | "already_funded";
    readonly deposit?: StressWalletDepositResult;
  }> = [];

  for (const record of records) {
    const before = beforeUtxos.get(record.envName) ?? [];
    if (
      options.forceFundExisting !== true &&
      fundingUtxos(before, options.lovelacePerWallet).length > 0
    ) {
      pendingEntries.push({ record, status: "already_funded" });
      continue;
    }
    const deposit = await runtime.submitDeposit({
      wallet: record,
      lovelace: options.lovelacePerWallet,
    });
    pendingEntries.push({ record, status: "submitted", deposit });
  }

  if (pendingEntries.some((entry) => entry.status === "submitted")) {
    if (projectionWaitMs > 0) {
      await sleepImpl(projectionWaitMs);
    }
    await runtime.projectDeposits();
  }

  const startedVerification = monotonicNow();
  let afterUtxos = new Map<string, readonly NodeUtxo[]>();
  while (true) {
    afterUtxos = await queryWalletUtxos({
      records,
      nodeEndpoint,
      fetchUtxos,
    });
    const allVerified = pendingEntries.every((entry) => {
      const before = beforeUtxos.get(entry.record.envName) ?? [];
      const after = afterUtxos.get(entry.record.envName) ?? [];
      return entry.status === "already_funded"
        ? fundingUtxos(after, options.lovelacePerWallet).length > 0
        : newFundingUtxos({
            before,
            after,
            lovelacePerWallet: options.lovelacePerWallet,
          }).length > 0;
    });
    if (allVerified) {
      break;
    }
    if (monotonicNow() - startedVerification >= verifyTimeoutMs) {
      const missing = pendingEntries
        .filter((entry) => {
          const before = beforeUtxos.get(entry.record.envName) ?? [];
          const after = afterUtxos.get(entry.record.envName) ?? [];
          return entry.status === "already_funded"
            ? fundingUtxos(after, options.lovelacePerWallet).length === 0
            : newFundingUtxos({
                before,
                after,
                lovelacePerWallet: options.lovelacePerWallet,
              }).length === 0;
        })
        .map((entry) => `${entry.record.envName}:${entry.record.l2Address}`);
      throw new Error(
        `Timed out verifying stress wallet funding for ${missing.join(", ")}.`,
      );
    }
    await sleepImpl(pollIntervalMs);
    await runtime.projectDeposits();
  }

  const preparedAt = now().toISOString();
  const entries = await Promise.all(
    pendingEntries.map(async (entry): Promise<StressWalletPrepareEntry> => {
      const before = beforeUtxos.get(entry.record.envName) ?? [];
      const after = afterUtxos.get(entry.record.envName) ?? [];
      const verifiedFunding =
        entry.status === "already_funded"
          ? fundingUtxos(after, options.lovelacePerWallet)
          : newFundingUtxos({
              before,
              after,
              lovelacePerWallet: options.lovelacePerWallet,
            });
      const latestFunding: StressWalletFundingSnapshot = {
        preparedAt,
        status: entry.status,
        lovelacePerWallet: options.lovelacePerWallet.toString(10),
        nodeEndpoint,
        beforeUtxoCount: before.length,
        afterUtxoCount: after.length,
        verifiedFundingUtxoCount: verifiedFunding.length,
        fundingUtxos: verifiedFunding.map((utxo) => ({
          outref: outRefKey(utxo),
          outputCbor: utxo.outputCbor.toString("hex"),
          lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
        })),
        ...(entry.deposit === undefined
          ? {}
          : { depositTxHash: entry.deposit.txHash }),
        ...(entry.deposit?.depositEventId === undefined
          ? {}
          : { depositEventId: entry.deposit.depositEventId }),
      };
      const updatedRecord: StressWalletRecord = {
        ...entry.record,
        latestFunding,
      };
      const path = pathsByEnv.get(entry.record.envName);
      if (path === undefined) {
        throw new Error(
          `Missing path for stress wallet ${entry.record.envName}.`,
        );
      }
      await writeStressWalletRecord(path, updatedRecord);
      return {
        wallet: summaryForRecord(updatedRecord, path),
        status: entry.status,
        beforeUtxoCount: before.length,
        afterUtxoCount: after.length,
        verifiedFundingUtxoCount: verifiedFunding.length,
        ...(entry.deposit === undefined
          ? {}
          : { depositTxHash: entry.deposit.txHash }),
        ...(entry.deposit?.depositEventId === undefined
          ? {}
          : { depositEventId: entry.deposit.depositEventId }),
      };
    }),
  );

  return {
    schemaVersion: STRESS_WALLET_PREPARE_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    generatedWalletCount: resolved.filter((wallet) => wallet.created).length,
    submittedDepositCount: entries.filter(
      (entry) => entry.status === "submitted",
    ).length,
    alreadyFundedCount: entries.filter(
      (entry) => entry.status === "already_funded",
    ).length,
    verifiedWalletCount: entries.length,
    lovelacePerWallet: options.lovelacePerWallet.toString(10),
    nodeEndpoint,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    wallets: entries,
  };
};

export const prepareStressWallets = async (
  options: PrepareStressWalletsOptions,
  runtime: PrepareStressWalletsRuntime,
): Promise<PrepareStressWalletsResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    prepareStressWalletsUnlocked(options, runtime),
  );
};

type FanoutNode = {
  readonly index: number;
  readonly record: StressWalletRecord;
  readonly parentIndex: number | null;
  readonly children: number[];
  readonly level: number;
  subtreeSize: number;
};

const acceptedTxStatuses = new Set([
  "accepted",
  "committed",
  "confirmed",
  "awaiting_local_recovery",
]);

const rejectedTxStatuses = new Set(["rejected", "failed"]);
const consolidationPendingTxStatuses = new Set([
  "not_found",
  "queued",
  "submitted",
  "validating",
  "accepted",
  "pending_commit",
  "awaiting_local_recovery",
]);

const nextFanoutPollDelayMs = ({
  attempt,
  initialMs,
  maxMs,
}: {
  readonly attempt: number;
  readonly initialMs: number;
  readonly maxMs: number;
}): number => Math.min(maxMs, initialMs * 2 ** attempt);

export const runBounded = async <T>(
  items: readonly T[],
  concurrency: number,
  task: (item: T) => Promise<void>,
): Promise<void> => {
  let next = 0;
  let failed = false;
  let firstError: unknown;
  const workers = Array.from(
    {
      length: Math.min(concurrency, items.length),
    },
    async () => {
      while (!failed && next < items.length) {
        const item = items[next]!;
        next += 1;
        try {
          await task(item);
        } catch (error) {
          if (!failed) {
            failed = true;
            firstError = error;
          }
        }
      }
    },
  );
  await Promise.all(workers);
  if (failed) {
    throw firstError;
  }
};

const buildFanoutNodes = ({
  records,
  branchFactor,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly branchFactor: number;
}): readonly FanoutNode[] => {
  const nodes: FanoutNode[] = records.map((record, index) => {
    const parentIndex =
      index < branchFactor
        ? null
        : Math.floor((index - branchFactor) / branchFactor);
    return {
      index,
      record,
      parentIndex,
      children: [],
      level: parentIndex === null ? 1 : 0,
      subtreeSize: 1,
    };
  });
  for (const node of nodes) {
    if (node.parentIndex !== null) {
      nodes[node.parentIndex]!.children.push(node.index);
    }
  }
  for (const node of nodes) {
    if (node.parentIndex !== null) {
      continue;
    }
    const stack = [node.index];
    while (stack.length > 0) {
      const current = nodes[stack.pop()!]!;
      for (const child of current.children) {
        const childNode = nodes[child]!;
        Object.assign(childNode, { level: current.level + 1 });
        stack.push(child);
      }
    }
  }
  for (let index = nodes.length - 1; index >= 0; index -= 1) {
    const node = nodes[index]!;
    if (node.parentIndex !== null) {
      nodes[node.parentIndex]!.subtreeSize += node.subtreeSize;
    }
  }
  return nodes;
};

const subtreeBudget = ({
  subtreeSize,
  lovelacePerWallet,
  feeHeadroomLovelace,
}: {
  readonly subtreeSize: number;
  readonly lovelacePerWallet: bigint;
  readonly feeHeadroomLovelace: bigint;
}): bigint =>
  BigInt(subtreeSize) * lovelacePerWallet +
  BigInt(Math.max(0, subtreeSize - 1)) * feeHeadroomLovelace;

const waitForFanoutAcceptance = async ({
  nodeEndpoint,
  txHash,
  runtime,
  acceptanceTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
}: {
  readonly nodeEndpoint: string;
  readonly txHash: string;
  readonly runtime: StressWalletFanoutRuntime;
  readonly acceptanceTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
}): Promise<string> => {
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const status = (await runtime.fetchTxStatus(nodeEndpoint, txHash)).trim();
    if (acceptedTxStatuses.has(status)) {
      return status;
    }
    if (rejectedTxStatuses.has(status)) {
      throw new Error(
        `Fanout transfer ${txHash} reached rejected status ${status}.`,
      );
    }
    if (monotonicNow() - startedAt >= acceptanceTimeoutMs) {
      throw new Error(
        `Timed out waiting ${acceptanceTimeoutMs.toString()}ms for fanout transfer ${txHash} to reach accepted status; last status ${status}.`,
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};

const verifiedFundingSnapshots = (
  utxos: readonly NodeUtxo[],
  lovelacePerWallet: bigint,
): readonly StressWalletFundingUtxoSnapshot[] =>
  fundingUtxos(utxos, lovelacePerWallet).map((utxo) => ({
    outref: outRefKey(utxo),
    outputCbor: utxo.outputCbor.toString("hex"),
    lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
  }));

const firstFundingUtxo = (
  utxos: readonly NodeUtxo[],
  minimumLovelace: bigint,
): NodeUtxo | undefined => fundingUtxos(utxos, minimumLovelace)[0];

const fetchFanoutUtxosWithRetry = async ({
  nodeEndpoint,
  address,
  fetchUtxos,
  sleep: sleepImpl,
}: {
  readonly nodeEndpoint: string;
  readonly address: string;
  readonly fetchUtxos: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep: (ms: number) => Promise<void>;
}): Promise<readonly NodeUtxo[]> => {
  let lastError: unknown;
  for (
    let attempt = 0;
    attempt < FANOUT_UTXO_QUERY_MAX_ATTEMPTS;
    attempt += 1
  ) {
    try {
      return await fetchUtxos(nodeEndpoint, address);
    } catch (error) {
      lastError = error;
      if (attempt === FANOUT_UTXO_QUERY_MAX_ATTEMPTS - 1) {
        break;
      }
      await sleepImpl(
        nextFanoutPollDelayMs({
          attempt,
          initialMs: FANOUT_UTXO_QUERY_INITIAL_RETRY_MS,
          maxMs: FANOUT_UTXO_QUERY_MAX_RETRY_MS,
        }),
      );
    }
  }
  throw lastError instanceof Error ? lastError : new Error(String(lastError));
};

const fanoutStressWalletsUnlocked = async (
  options: FanoutStressWalletsOptions,
  runtime: StressWalletFanoutRuntime,
): Promise<StressWalletFanoutResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.lovelacePerWallet <= 0n) {
    throw new Error("lovelacePerWallet must be greater than zero.");
  }
  const branchFactor = requireSafePositiveInteger(
    options.branchFactor ?? DEFAULT_FANOUT_BRANCH_FACTOR,
    "branchFactor",
  );
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_FANOUT_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const feeHeadroomLovelace =
    options.feeHeadroomLovelace ?? DEFAULT_FANOUT_FEE_HEADROOM_LOVELACE;
  if (feeHeadroomLovelace < 0n) {
    throw new Error("feeHeadroomLovelace must be non-negative.");
  }
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(
    options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "acceptanceTimeoutMs",
  );
  const pollInitialIntervalMs = requireSafePositiveInteger(
    options.pollInitialIntervalMs ?? 250,
    "pollInitialIntervalMs",
  );
  const pollMaxIntervalMs = requireSafePositiveInteger(
    options.pollMaxIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS,
    "pollMaxIntervalMs",
  );
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const now = runtime.now ?? options.now ?? (() => new Date());
  const nodeEndpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const fetchUtxos = runtime.fetchUtxos ?? fetchNodeUtxosByAddress;
  const sleepImpl = runtime.sleep ?? sleep;
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: options.createMissing === true,
    now,
    generateSeedPhrase: options.generateSeedPhrase ?? defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const exports = await writeStressWalletExports(outDir, records);
  const nodes = buildFanoutNodes({ records, branchFactor });
  const edges: StressWalletFanoutEdgeSummary[] = [];
  let submittedTransferCount = 0;
  let alreadyFundedTransferCount = 0;
  const levels = [...new Set(nodes.map((node) => node.level))].sort(
    (left, right) => left - right,
  );

  for (const level of levels) {
    const parents =
      level === 1
        ? [null]
        : nodes
            .filter(
              (node) => node.level === level - 1 && node.children.length > 0,
            )
            .map((node) => node.index);
    await runBounded(parents, maxInFlight, async (parentIndex) => {
      const childIndexes =
        parentIndex === null
          ? nodes
              .filter(
                (node) => node.parentIndex === null && node.level === level,
              )
              .map((node) => node.index)
          : nodes[parentIndex]!.children;
      for (const childIndex of childIndexes) {
        const child = nodes[childIndex]!;
        const source: StressWalletFanoutSource =
          parentIndex === null
            ? {
                kind: "treasury",
                seedPhrase: options.treasurySeedPhrase,
                walletId: "treasury",
              }
            : { kind: "wallet", wallet: nodes[parentIndex]!.record };
        const lovelace = subtreeBudget({
          subtreeSize: child.subtreeSize,
          lovelacePerWallet: options.lovelacePerWallet,
          feeHeadroomLovelace,
        });
        const existingFunding = firstFundingUtxo(
          await fetchFanoutUtxosWithRetry({
            nodeEndpoint,
            address: child.record.l2Address,
            fetchUtxos,
            sleep: sleepImpl,
          }),
          options.lovelacePerWallet,
        );
        if (existingFunding !== undefined) {
          alreadyFundedTransferCount += 1;
          edges.push({
            level,
            parentWalletId:
              parentIndex === null
                ? "treasury"
                : nodes[parentIndex]!.record.walletId,
            childWalletId: child.record.walletId,
            lovelace: lovelace.toString(10),
            txHash: existingFunding.txHash,
            acceptedStatus: "already_funded",
            submitted: false,
          });
          continue;
        }
        const submitted = await runtime.submitTransfer({
          source,
          destination: child.record,
          lovelace,
          level,
        });
        submittedTransferCount += 1;
        const acceptedStatus = await waitForFanoutAcceptance({
          nodeEndpoint,
          txHash: submitted.txHash,
          runtime,
          acceptanceTimeoutMs,
          pollInitialIntervalMs,
          pollMaxIntervalMs,
        });
        edges.push({
          level,
          parentWalletId:
            parentIndex === null
              ? "treasury"
              : nodes[parentIndex]!.record.walletId,
          childWalletId: child.record.walletId,
          lovelace: lovelace.toString(10),
          txHash: submitted.txHash,
          acceptedStatus,
          submitted: true,
        });
      }
    });
  }

  const preparedAt = now().toISOString();
  const pathsByEnv = new Map(
    resolved.map(({ path, record }) => [record.envName, path] as const),
  );
  const walletEntries: StressWalletFanoutEntry[] = new Array(records.length);
  await runBounded(
    records.map((record, index) => ({ record, index })),
    maxInFlight,
    async ({ record, index }) => {
      const snapshots = verifiedFundingSnapshots(
        await fetchFanoutUtxosWithRetry({
          nodeEndpoint,
          address: record.l2Address,
          fetchUtxos,
          sleep: sleepImpl,
        }),
        options.lovelacePerWallet,
      );
      if (snapshots.length === 0) {
        throw new Error(
          `Fanout did not verify a funding UTxO with at least ${options.lovelacePerWallet.toString(10)} lovelace for ${record.envName}:${record.l2Address}.`,
        );
      }
      const updatedRecord: StressWalletRecord = {
        ...record,
        latestFunding: {
          preparedAt,
          status: "submitted",
          lovelacePerWallet: options.lovelacePerWallet.toString(10),
          nodeEndpoint,
          beforeUtxoCount: 0,
          afterUtxoCount: snapshots.length,
          verifiedFundingUtxoCount: snapshots.length,
          fundingUtxos: snapshots,
        },
      };
      const path = pathsByEnv.get(record.envName);
      if (path === undefined) {
        throw new Error(`Missing path for stress wallet ${record.envName}.`);
      }
      await writeStressWalletRecord(path, updatedRecord);
      walletEntries[index] = {
        wallet: summaryForRecord(updatedRecord, path),
        verifiedFundingUtxoCount: snapshots.length,
      };
    },
  );
  const levelSummaries = levels.map((level) => ({
    level,
    transferCount: edges.filter((edge) => edge.level === level).length,
  }));
  const rootRequiredLovelace =
    BigInt(records.length) * options.lovelacePerWallet +
    BigInt(records.length) * feeHeadroomLovelace;
  const reportPath = join(outDir, "fanout-report.json");
  const result: StressWalletFanoutResult = {
    schemaVersion: STRESS_WALLET_FANOUT_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    generatedWalletCount: resolved.filter((wallet) => wallet.created).length,
    branchFactor,
    maxInFlight,
    lovelacePerWallet: options.lovelacePerWallet.toString(10),
    feeHeadroomLovelace: feeHeadroomLovelace.toString(10),
    rootRequiredLovelace: rootRequiredLovelace.toString(10),
    submittedTransferCount,
    alreadyFundedTransferCount,
    verifiedWalletCount: walletEntries.length,
    nodeEndpoint,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    reportPath,
    levels: levelSummaries,
    wallets: walletEntries,
  };
  await writeFile(
    reportPath,
    `${formatJson({
      ...result,
      edges,
    })}\n`,
    "utf8",
  );
  return result;
};

export const fanoutStressWallets = async (
  options: FanoutStressWalletsOptions,
  runtime: StressWalletFanoutRuntime,
): Promise<StressWalletFanoutResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    fanoutStressWalletsUnlocked(options, runtime),
  );
};

export type StressWalletOperationScope = {
  readonly count: number;
  readonly startIndex: number;
  readonly envPrefix: string;
  readonly network: Network;
  readonly walletSetSha256: string;
};

const buildStressWalletOperationScope = ({
  records,
  count,
  startIndex,
  envPrefix,
  network,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly count: number;
  readonly startIndex: number;
  readonly envPrefix: string;
  readonly network: Network;
}): StressWalletOperationScope => {
  const walletSet = records.map((record) => ({
    walletId: record.walletId,
    index: record.index,
    envName: record.envName,
    l2Address: record.l2Address,
    paymentKeyHash: record.paymentKeyHash,
  }));
  return {
    count,
    startIndex,
    envPrefix,
    network,
    walletSetSha256: createHash("sha256")
      .update(JSON.stringify(walletSet))
      .digest("hex"),
  };
};

const sameStressWalletOperationScope = (
  left: StressWalletOperationScope,
  right: StressWalletOperationScope,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const computeSignedNativeTxHash = (
  signedTxCbor: string,
  walletId: string,
): string => {
  try {
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(signedTxCbor, "hex"),
    );
    return computeMidgardNativeTxId(decoded).toString("hex");
  } catch (cause) {
    throw new Error(
      `Consolidation state signedTxCbor for ${walletId} is not canonical Midgard native transaction CBOR: ${String(cause)}`,
    );
  }
};

const parseStressWalletOperationScope = (
  value: unknown,
): StressWalletOperationScope => {
  const raw = asObject(value, "scope");
  const walletSetSha256 = requiredString(
    raw.walletSetSha256,
    "scope.walletSetSha256",
  );
  if (!/^[0-9a-f]{64}$/.test(walletSetSha256)) {
    throw new Error(
      "scope.walletSetSha256 must be a lowercase SHA-256 digest.",
    );
  }
  return {
    count: requiredPositiveInteger(raw.count, "scope.count"),
    startIndex: requiredPositiveInteger(raw.startIndex, "scope.startIndex"),
    envPrefix: requiredString(raw.envPrefix, "scope.envPrefix"),
    network: parseStressWalletNetwork(
      requiredString(raw.network, "scope.network"),
      {},
    ),
    walletSetSha256,
  };
};

type ConsolidationStateEntry = {
  readonly walletId: string;
  readonly address: string;
  readonly beforeLovelace: string;
  readonly beforeOutrefs: readonly string[];
  readonly requestedLovelace: string;
  readonly txHash?: string;
  readonly signedTxCbor?: string;
  readonly selectedInputs?: readonly string[];
  readonly selectedInputLovelace?: string;
  readonly acceptedStatus?: string;
};

type ConsolidationState = {
  readonly schemaVersion: typeof STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION;
  readonly treasuryAddress: string;
  readonly nodeEndpoint: string;
  readonly reserveLovelace: string;
  readonly scope: StressWalletOperationScope;
  readonly entries: readonly ConsolidationStateEntry[];
};

const assertConsolidationTransferIntent = ({
  entry,
  source,
  treasuryAddress,
  reserveLovelace,
  network,
}: {
  readonly entry: ConsolidationStateEntry;
  readonly source: StressWalletRecord;
  readonly treasuryAddress: string;
  readonly reserveLovelace: bigint;
  readonly network: Network;
}): void => {
  if (
    entry.txHash === undefined ||
    entry.signedTxCbor === undefined ||
    entry.selectedInputs === undefined ||
    entry.selectedInputLovelace === undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is incomplete.`,
    );
  }
  let nativeTx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>;
  let tx: ReturnType<typeof decodeMidgardLedgerTxFromCanonicalCbor>;
  try {
    const signedTxCbor = Buffer.from(entry.signedTxCbor, "hex");
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(signedTxCbor);
    tx = decodeMidgardLedgerTxFromCanonicalCbor(signedTxCbor);
  } catch (cause) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not decodable: ${String(cause)}`,
    );
  }
  if (tx.txId.toString("hex") !== entry.txHash) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has a tx hash mismatch.`,
    );
  }
  const expectedNetworkId = network === "Mainnet" ? 1n : 0n;
  if (tx.networkId !== expectedNetworkId) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has network id ${String(tx.networkId)}, expected ${expectedNetworkId.toString()}.`,
    );
  }
  if (tx.validity !== "TxIsValid") {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is marked invalid.`,
    );
  }
  if (
    nativeTx.version !== MIDGARD_NATIVE_TX_VERSION ||
    nativeTx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
    nativeTx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE ||
    tx.validityIntervalStart !== undefined ||
    tx.validityIntervalEnd !== undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must use the supported native version with unbounded validity intervals.`,
    );
  }
  const decodedInputs = tx.spendInputs
    .map((input) => `${input.txId.toString("hex")}#${input.index.toString()}`)
    .sort();
  const journaledInputs = [...entry.selectedInputs].sort();
  if (
    decodedInputs.length !== journaledInputs.length ||
    decodedInputs.some((input, index) => input !== journaledInputs[index])
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not spend exactly its journaled selected inputs.`,
    );
  }
  if (
    new Set(journaledInputs).size !== journaledInputs.length ||
    journaledInputs.some((input) => !entry.beforeOutrefs.includes(input))
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} selects inputs outside its exact source snapshot.`,
    );
  }
  const requestedLovelace = BigInt(entry.requestedLovelace);
  const selectedInputLovelace = BigInt(entry.selectedInputLovelace);
  if (tx.fee < 0n || tx.fee > reserveLovelace) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has fee ${tx.fee.toString()} outside reserve ${reserveLovelace.toString()}.`,
    );
  }
  const expectedChange = selectedInputLovelace - requestedLovelace - tx.fee;
  if (expectedChange < 0n) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not conserve selected input lovelace.`,
    );
  }
  let treasuryOutputCount = 0;
  let sourceOutputCount = 0;
  let sourceChange = 0n;
  for (const output of tx.outputs) {
    const outputAddress = encodeMidgardAddressText(output.address);
    if (
      output.datum !== undefined ||
      output.scriptRef !== undefined ||
      output.value.assets.size !== 0
    ) {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} contains datum, script, or non-ADA output content.`,
      );
    }
    if (outputAddress === treasuryAddress) {
      treasuryOutputCount += 1;
      if (output.value.lovelace !== requestedLovelace) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has the wrong treasury value.`,
        );
      }
    } else if (outputAddress === source.l2Address) {
      sourceOutputCount += 1;
      sourceChange += output.value.lovelace;
    } else {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} pays an unexpected address ${outputAddress}.`,
      );
    }
  }
  if (treasuryOutputCount !== 1 || sourceOutputCount > 1) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must contain exactly one treasury output and at most one source change output.`,
    );
  }
  if (sourceChange !== expectedChange) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has source change ${sourceChange.toString()}, expected ${expectedChange.toString()}.`,
    );
  }
  const expectedSigner = source.paymentKeyHash;
  const requiredSigners = tx.requiredSignerHashes.map((hash) =>
    hash.toString("hex"),
  );
  const witnessSigners = tx.witnessKeyHashes.map((hash) =>
    hash.toString("hex"),
  );
  if (
    requiredSigners.length !== 1 ||
    requiredSigners[0] !== expectedSigner ||
    witnessSigners.length !== 1 ||
    witnessSigners[0] !== expectedSigner ||
    tx.vkeyWitnesses.length !== 1
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not signed solely by its source payment key.`,
    );
  }
  const witness = tx.vkeyWitnesses[0]!;
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try {
    const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try {
      if (!publicKey.verify(tx.txId, signature)) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has an invalid source signature.`,
        );
      }
    } finally {
      signature.free();
    }
  } finally {
    publicKey.free();
  }
  if (
    tx.referenceInputs.length !== 0 ||
    tx.requiredObserverHashes.length !== 0 ||
    tx.scriptWitnesses.length !== 0 ||
    tx.nativeScriptHashes.length !== 0 ||
    tx.plutusScriptHashes.length !== 0 ||
    tx.redeemers.length !== 0 ||
    tx.mint.assets.length !== 0 ||
    tx.requiresPlutusEvaluation ||
    !tx.auxiliaryDataHash.equals(EMPTY_NULL_ROOT) ||
    !tx.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} contains forbidden reference, mint, observer, auxiliary, or script content.`,
    );
  }
};

const sumLovelace = (utxos: readonly NodeUtxo[]): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

const utxoAccounting = (utxos: readonly NodeUtxo[]) => ({
  lovelace: sumLovelace(utxos).toString(10),
  utxoCount: utxos.length,
  outrefs: utxos.map(outRefKey).sort(),
});

const readConsolidationState = async (
  path: string,
): Promise<ConsolidationState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  const raw = asObject(
    JSON.parse(await readFile(path, "utf8")) as unknown,
    "stress wallet consolidation state",
  );
  if (raw.schemaVersion !== STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported consolidation state schema at ${path}; run the explicit stress-wallets:consolidate-upgrade-state command for a legacy journal.`,
    );
  }
  if (!Array.isArray(raw.entries)) {
    throw new Error(`Consolidation state entries at ${path} must be an array.`);
  }
  const walletIds = new Set<string>();
  const txHashes = new Set<string>();
  const entries = raw.entries.map((value, index): ConsolidationStateEntry => {
    const entry = asObject(value, `entries[${index.toString()}]`);
    if (!Array.isArray(entry.beforeOutrefs)) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].beforeOutrefs must be an array.`,
      );
    }
    if (
      entry.selectedInputs !== undefined &&
      !Array.isArray(entry.selectedInputs)
    ) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].selectedInputs must be an array.`,
      );
    }
    const parsed: ConsolidationStateEntry = {
      walletId: requiredString(entry.walletId, "walletId"),
      address: requiredString(entry.address, "address"),
      beforeLovelace: requiredString(entry.beforeLovelace, "beforeLovelace"),
      beforeOutrefs: entry.beforeOutrefs.map((outref) =>
        requiredString(outref, "beforeOutref"),
      ),
      requestedLovelace: requiredString(
        entry.requestedLovelace,
        "requestedLovelace",
      ),
      ...(entry.txHash === undefined
        ? {}
        : { txHash: requiredString(entry.txHash, "txHash") }),
      ...(entry.signedTxCbor === undefined
        ? {}
        : {
            signedTxCbor: requiredString(entry.signedTxCbor, "signedTxCbor"),
          }),
      ...(entry.selectedInputs === undefined
        ? {}
        : {
            selectedInputs: entry.selectedInputs.map((input) =>
              requiredString(input, "selectedInput"),
            ),
          }),
      ...(entry.selectedInputLovelace === undefined
        ? {}
        : {
            selectedInputLovelace: requiredString(
              entry.selectedInputLovelace,
              "selectedInputLovelace",
            ),
          }),
      ...(entry.acceptedStatus === undefined
        ? {}
        : {
            acceptedStatus: requiredString(
              entry.acceptedStatus,
              "acceptedStatus",
            ),
          }),
    };
    if (walletIds.has(parsed.walletId)) {
      throw new Error(
        `Duplicate walletId ${parsed.walletId} in consolidation state at ${path}.`,
      );
    }
    walletIds.add(parsed.walletId);
    if (parsed.txHash !== undefined) {
      if (!/^[0-9a-f]{64}$/.test(parsed.txHash)) {
        throw new Error(
          `Consolidation state txHash for ${parsed.walletId} must be a lowercase 32-byte digest.`,
        );
      }
      if (txHashes.has(parsed.txHash)) {
        throw new Error(
          `Duplicate txHash ${parsed.txHash} in consolidation state at ${path}.`,
        );
      }
      txHashes.add(parsed.txHash);
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (!/^[0-9a-f]+$/.test(parsed.signedTxCbor) ||
        parsed.signedTxCbor.length % 2 !== 0)
    ) {
      throw new Error(
        `Consolidation state signedTxCbor for ${parsed.walletId} must be non-empty lowercase hex.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      parsed.txHash !== undefined &&
      computeSignedNativeTxHash(parsed.signedTxCbor, parsed.walletId) !==
        parsed.txHash
    ) {
      throw new Error(
        `Consolidation state txHash/signedTxCbor mismatch for ${parsed.walletId}.`,
      );
    }
    if (parsed.signedTxCbor !== undefined && parsed.txHash === undefined) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without txHash.`,
      );
    }
    if (
      parsed.selectedInputLovelace !== undefined &&
      !/^(0|[1-9]\d*)$/.test(parsed.selectedInputLovelace)
    ) {
      throw new Error(
        `Consolidation state selectedInputLovelace for ${parsed.walletId} must be a canonical non-negative decimal.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (parsed.selectedInputs === undefined ||
        parsed.selectedInputLovelace === undefined)
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without exact selected input accounting.`,
      );
    }
    if (
      parsed.signedTxCbor === undefined &&
      parsed.selectedInputLovelace !== undefined
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has selected input accounting without signedTxCbor.`,
      );
    }
    if (
      parsed.txHash !== undefined &&
      parsed.signedTxCbor === undefined &&
      parsed.acceptedStatus !== "committed"
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} lacks signedTxCbor and is not a committed legacy-upgrade checkpoint.`,
      );
    }
    return parsed;
  });
  return {
    schemaVersion: STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION,
    treasuryAddress: requiredString(raw.treasuryAddress, "treasuryAddress"),
    nodeEndpoint: requiredString(raw.nodeEndpoint, "nodeEndpoint"),
    reserveLovelace: requiredString(raw.reserveLovelace, "reserveLovelace"),
    scope: parseStressWalletOperationScope(raw.scope),
    entries,
  };
};

export type UpgradeLegacyConsolidationStateOptions = {
  readonly count: number;
  readonly expectedEntryCount: number;
  readonly expectedPreimageSha256: string;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly reserveLovelace?: bigint;
  readonly maxInFlight?: number;
  readonly requestTimeoutMs?: number;
  readonly now?: () => Date;
};

export type UpgradeLegacyConsolidationStateRuntime = {
  readonly fetchTxStatus: (
    nodeEndpoint: string,
    txHash: string,
  ) => Promise<string>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly now?: () => Date;
};

export type UpgradeLegacyConsolidationStateResult = {
  readonly schemaVersion: "midgard-stress-wallet-consolidation-upgrade-v1";
  readonly statePath: string;
  readonly snapshotPath: string;
  readonly preparedAuditPath: string;
  readonly auditPath: string;
  readonly preimageSha256: string;
  readonly postimageSha256: string;
  readonly entryCount: number;
  readonly scope: StressWalletOperationScope;
};

const parseCanonicalLegacyLovelace = (
  value: unknown,
  fieldName: string,
): bigint => {
  const encoded = requiredString(value, fieldName);
  if (!/^(0|[1-9][0-9]*)$/.test(encoded)) {
    throw new Error(
      `${fieldName} must be canonical non-negative decimal lovelace.`,
    );
  }
  return BigInt(encoded);
};

const parseLegacyConsolidationEntries = (
  rawEntries: unknown,
  path: string,
  reserveLovelace: bigint,
): readonly ConsolidationStateEntry[] => {
  if (!Array.isArray(rawEntries)) {
    throw new Error(
      `Legacy consolidation state entries at ${path} must be an array.`,
    );
  }
  const walletIds = new Set<string>();
  const txHashes = new Set<string>();
  return rawEntries.map((value, index) => {
    const raw = asObject(value, `entries[${index.toString()}]`);
    if (!Array.isArray(raw.beforeOutrefs)) {
      throw new Error(
        `Legacy entries[${index.toString()}].beforeOutrefs must be an array.`,
      );
    }
    if (!Array.isArray(raw.selectedInputs) || raw.selectedInputs.length === 0) {
      throw new Error(
        `Legacy entry ${index.toString()} has no selectedInputs; refusing an ambiguous upgrade.`,
      );
    }
    const selectedInputs = raw.selectedInputs.map((input) =>
      requiredString(input, "selectedInput"),
    );
    const txHash = requiredString(raw.txHash, "txHash");
    const beforeLovelace = parseCanonicalLegacyLovelace(
      raw.beforeLovelace,
      "beforeLovelace",
    );
    const requestedLovelace = parseCanonicalLegacyLovelace(
      raw.requestedLovelace,
      "requestedLovelace",
    );
    const expectedRequested =
      beforeLovelace > reserveLovelace ? beforeLovelace - reserveLovelace : 0n;
    if (requestedLovelace !== expectedRequested) {
      throw new Error(
        `Legacy entry ${String(raw.walletId)} requestedLovelace does not equal beforeLovelace minus reserve.`,
      );
    }
    const entry: ConsolidationStateEntry = {
      walletId: requiredString(raw.walletId, "walletId"),
      address: requiredString(raw.address, "address"),
      beforeLovelace: beforeLovelace.toString(10),
      beforeOutrefs: raw.beforeOutrefs.map((outref) =>
        requiredString(outref, "beforeOutref"),
      ),
      requestedLovelace: requestedLovelace.toString(10),
      txHash,
      selectedInputs,
      acceptedStatus: "committed",
    };
    if (!/^[0-9a-f]{64}$/.test(txHash)) {
      throw new Error(`Legacy entry ${entry.walletId} has an invalid txHash.`);
    }
    if (walletIds.has(entry.walletId) || txHashes.has(txHash)) {
      throw new Error(
        "Legacy consolidation state contains duplicate walletId or txHash.",
      );
    }
    walletIds.add(entry.walletId);
    txHashes.add(txHash);
    const beforeSet = new Set(entry.beforeOutrefs);
    if (
      beforeSet.size !== entry.beforeOutrefs.length ||
      new Set(selectedInputs).size !== selectedInputs.length ||
      selectedInputs.some((input) => !beforeSet.has(input))
    ) {
      throw new Error(
        `Legacy entry ${entry.walletId} selectedInputs must be a unique subset of journaled source outrefs.`,
      );
    }
    return entry;
  });
};

export type LegacyLiveSourceClassification = {
  readonly walletId: string;
  readonly txHash: string;
  readonly lovelace: string;
  readonly producedPositiveOutrefs: readonly string[];
  readonly producedZeroOutrefs: readonly string[];
  readonly unselectedBeforeTombstoneOutrefs: readonly string[];
};

const classifyLegacyLiveSource = (
  entry: ConsolidationStateEntry,
  sourceUtxos: readonly NodeUtxo[],
  reserveLovelace: bigint,
): LegacyLiveSourceClassification => {
  const txHash = entry.txHash;
  const selectedInputs = entry.selectedInputs;
  if (txHash === undefined || selectedInputs === undefined) {
    throw new Error(
      "Legacy entry " + entry.walletId + " lacks an exact txHash/input proof.",
    );
  }
  const beforeSet = new Set(entry.beforeOutrefs);
  const selectedSet = new Set(selectedInputs);
  const producedPositiveOutrefs: string[] = [];
  const producedZeroOutrefs: string[] = [];
  const unselectedBeforeTombstoneOutrefs: string[] = [];
  for (const utxo of sourceUtxos) {
    const outref = outRefKey(utxo);
    const hasNonAda = Object.entries(utxo.assets).some(
      ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
    );
    if (hasNonAda) {
      throw new Error(
        "Legacy entry " +
          entry.walletId +
          " live source contains non-ADA assets at " +
          outref +
          ".",
      );
    }
    if (selectedSet.has(outref)) {
      throw new Error(
        "Legacy entry " +
          entry.walletId +
          " still contains selected input " +
          outref +
          ".",
      );
    }
    const hasPositiveValue = Object.values(utxo.assets).some(
      (quantity) => quantity > 0n,
    );
    const hasAnyValue = Object.values(utxo.assets).some(
      (quantity) => quantity !== 0n,
    );
    if (utxo.txHash === txHash) {
      if (hasPositiveValue) {
        producedPositiveOutrefs.push(outref);
      } else if (!hasAnyValue) {
        producedZeroOutrefs.push(outref);
      } else {
        throw new Error(
          "Legacy entry " +
            entry.walletId +
            " live source has invalid value at " +
            outref +
            ".",
        );
      }
      continue;
    }
    if (
      beforeSet.has(outref) &&
      (utxo.assets.lovelace ?? 0n) === 1n &&
      Object.entries(utxo.assets).every(
        ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
      )
    ) {
      unselectedBeforeTombstoneOutrefs.push(outref);
      continue;
    }
    throw new Error(
      "Legacy entry " +
        entry.walletId +
        " live source contains unknown or positive foreign outref " +
        outref +
        ".",
    );
  }
  const lovelace = sumLovelace(sourceUtxos);
  if (lovelace > reserveLovelace) {
    throw new Error(
      "Legacy entry " +
        entry.walletId +
        " live source lovelace " +
        lovelace.toString(10) +
        " exceeds reserve " +
        reserveLovelace.toString(10) +
        ".",
    );
  }
  if (producedPositiveOutrefs.length === 0) {
    throw new Error(
      "Legacy entry " +
        entry.walletId +
        " has no positive live source output produced by " +
        txHash +
        ".",
    );
  }
  return {
    walletId: entry.walletId,
    txHash,
    lovelace: lovelace.toString(10),
    producedPositiveOutrefs: producedPositiveOutrefs.sort(),
    producedZeroOutrefs: producedZeroOutrefs.sort(),
    unselectedBeforeTombstoneOutrefs: unselectedBeforeTombstoneOutrefs.sort(),
  };
};

type ValidatedLegacyConsolidationState = {
  readonly preimage: Buffer;
  readonly preimageSha256: string;
  readonly entries: readonly ConsolidationStateEntry[];
  readonly classifications: readonly LegacyLiveSourceClassification[];
  readonly records: readonly StressWalletRecord[];
  readonly scope: StressWalletOperationScope;
  readonly treasuryAddress: string;
  readonly nodeEndpoint: string;
  readonly reserveLovelace: bigint;
  readonly statePath: string;
  readonly outDir: string;
  readonly now: () => Date;
};

const validateLegacyConsolidationState = async (
  options: UpgradeLegacyConsolidationStateOptions,
  runtime: UpgradeLegacyConsolidationStateRuntime,
): Promise<ValidatedLegacyConsolidationState> => {
  requireSafePositiveInteger(options.count, "count");
  const expectedEntryCount = requireSafePositiveInteger(
    options.expectedEntryCount,
    "expectedEntryCount",
  );
  if (!/^[0-9a-f]{64}$/.test(options.expectedPreimageSha256)) {
    throw new Error(
      "expectedPreimageSha256 must be a lowercase SHA-256 digest.",
    );
  }
  const reserveLovelace =
    options.reserveLovelace ?? DEFAULT_CONSOLIDATE_RESERVE_LOVELACE;
  if (reserveLovelace < 0n) {
    throw new Error("reserveLovelace must be non-negative.");
  }
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const requestTimeoutMs = requireSafePositiveInteger(
    options.requestTimeoutMs ?? 30_000,
    "requestTimeoutMs",
  );
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const now = runtime.now ?? options.now ?? (() => new Date());
  const nodeEndpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: false,
    now,
    generateSeedPhrase: defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const scope = buildStressWalletOperationScope({
    records,
    count: options.count,
    startIndex,
    envPrefix,
    network,
  });
  const treasuryAddress = deriveWalletInfo(
    { seedPhrase: options.treasurySeedPhrase, resolvedFrom: "treasury" },
    network,
  ).address;
  if (records.some((record) => record.l2Address === treasuryAddress)) {
    throw new Error(
      "Treasury address must be distinct from every stress wallet address.",
    );
  }
  const statePath = join(outDir, "consolidation-state.json");
  const preimage = await readFile(statePath);
  const preimageSha256 = createHash("sha256").update(preimage).digest("hex");
  if (preimageSha256 !== options.expectedPreimageSha256) {
    throw new Error(
      "Legacy consolidation preimage SHA-256 " +
        preimageSha256 +
        " does not match required " +
        options.expectedPreimageSha256 +
        ".",
    );
  }
  const raw = asObject(
    JSON.parse(preimage.toString("utf8")) as unknown,
    "legacy consolidation state",
  );
  if (raw.schemaVersion !== "midgard-stress-wallet-consolidate-v1") {
    throw new Error(
      "Expected legacy consolidation schema at " +
        statePath +
        "; found " +
        String(raw.schemaVersion) +
        ".",
    );
  }
  if (
    requiredString(raw.treasuryAddress, "treasuryAddress") !==
      treasuryAddress ||
    requiredString(raw.nodeEndpoint, "nodeEndpoint") !== nodeEndpoint ||
    requiredString(raw.reserveLovelace, "reserveLovelace") !==
      reserveLovelace.toString(10)
  ) {
    throw new Error(
      "Legacy consolidation state does not match the required treasury, endpoint, or reserve.",
    );
  }
  const entries = parseLegacyConsolidationEntries(
    raw.entries,
    statePath,
    reserveLovelace,
  );
  if (entries.length !== expectedEntryCount) {
    throw new Error(
      "Legacy consolidation entry count " +
        entries.length.toString() +
        " does not match required " +
        expectedEntryCount.toString() +
        ".",
    );
  }
  const recordsById = new Map(
    records.map((record) => [record.walletId, record]),
  );
  for (const entry of entries) {
    const record = recordsById.get(entry.walletId);
    if (record === undefined || record.l2Address !== entry.address) {
      throw new Error(
        "Legacy entry " +
          entry.walletId +
          " is outside the exact controlled wallet scope.",
      );
    }
  }
  const fetchUtxos =
    runtime.fetchUtxos ??
    ((endpoint: string, address: string) =>
      fetchNodeUtxosByAddress(endpoint, address, {
        timeoutMs: requestTimeoutMs,
      }));
  const classifications: LegacyLiveSourceClassification[] = new Array(
    entries.length,
  );
  await runBounded(
    entries.map((entry, index) => ({ entry, index })),
    maxInFlight,
    async ({ entry, index }) => {
      const txHash = entry.txHash!;
      const [status, sourceUtxos] = await Promise.all([
        runtime.fetchTxStatus(nodeEndpoint, txHash),
        fetchUtxos(nodeEndpoint, entry.address),
      ]);
      if (status.trim().toLowerCase() !== "committed") {
        throw new Error(
          "Legacy entry " +
            entry.walletId +
            " transaction " +
            txHash +
            " is not committed.",
        );
      }
      classifications[index] = classifyLegacyLiveSource(
        entry,
        sourceUtxos,
        reserveLovelace,
      );
    },
  );
  return {
    preimage,
    preimageSha256,
    entries,
    classifications,
    records,
    scope,
    treasuryAddress,
    nodeEndpoint,
    reserveLovelace,
    statePath,
    outDir,
    now,
  };
};

export type VerifyLegacyConsolidationStateOptions =
  UpgradeLegacyConsolidationStateOptions & {
    readonly reportPath: string;
  };

export type VerifyLegacyConsolidationStateResult = {
  readonly schemaVersion: "midgard-stress-wallet-consolidation-verification-v1";
  readonly reportPath: string;
  readonly preimageSha256: string;
  readonly entryCount: number;
  readonly totalLiveLovelace: string;
  readonly producedPositiveOutrefCount: number;
  readonly producedZeroOutrefCount: number;
  readonly unselectedBeforeTombstoneOutrefCount: number;
};

const verifyLegacyConsolidationStateUnlocked = async (
  options: VerifyLegacyConsolidationStateOptions,
  runtime: UpgradeLegacyConsolidationStateRuntime,
): Promise<VerifyLegacyConsolidationStateResult> => {
  const validated = await validateLegacyConsolidationState(options, runtime);
  const reportPath = options.reportPath.trim();
  if (!reportPath) {
    throw new Error("reportPath is required.");
  }
  if (await fileExists(reportPath)) {
    throw new Error(
      "Refusing to overwrite legacy verification report at " + reportPath + ".",
    );
  }
  const totalLiveLovelace = validated.classifications.reduce(
    (total, classification) => total + BigInt(classification.lovelace),
    0n,
  );
  const result: VerifyLegacyConsolidationStateResult = {
    schemaVersion: "midgard-stress-wallet-consolidation-verification-v1",
    reportPath,
    preimageSha256: validated.preimageSha256,
    entryCount: validated.entries.length,
    totalLiveLovelace: totalLiveLovelace.toString(10),
    producedPositiveOutrefCount: validated.classifications.reduce(
      (total, classification) =>
        total + classification.producedPositiveOutrefs.length,
      0,
    ),
    producedZeroOutrefCount: validated.classifications.reduce(
      (total, classification) =>
        total + classification.producedZeroOutrefs.length,
      0,
    ),
    unselectedBeforeTombstoneOutrefCount: validated.classifications.reduce(
      (total, classification) =>
        total + classification.unselectedBeforeTombstoneOutrefs.length,
      0,
    ),
  };
  await assertFileGeneration(validated.statePath, validated.preimageSha256);
  await writePrivateFileAtomicNoReplace(
    reportPath,
    formatJson({
      ...result,
      observedAt: validated.now().toISOString(),
      statePath: validated.statePath,
      scope: validated.scope,
      treasuryAddress: validated.treasuryAddress,
      nodeEndpoint: validated.nodeEndpoint,
      reserveLovelace: validated.reserveLovelace.toString(10),
      wallets: validated.classifications,
    }) + "\n",
  );
  return result;
};

export const verifyLegacyConsolidationState = async (
  options: VerifyLegacyConsolidationStateOptions,
  runtime: UpgradeLegacyConsolidationStateRuntime,
): Promise<VerifyLegacyConsolidationStateResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const statePath = join(outDir, "consolidation-state.json");
  return withExclusiveConsolidationStateLock(statePath, () =>
    verifyLegacyConsolidationStateUnlocked(options, runtime),
  );
};

const upgradeLegacyConsolidationStateUnlocked = async (
  options: UpgradeLegacyConsolidationStateOptions,
  runtime: UpgradeLegacyConsolidationStateRuntime,
): Promise<UpgradeLegacyConsolidationStateResult> => {
  const validated = await validateLegacyConsolidationState(options, runtime);
  const {
    preimage,
    preimageSha256,
    entries,
    scope,
    treasuryAddress,
    nodeEndpoint,
    reserveLovelace,
    statePath,
    outDir,
    now,
  } = validated;
  const postimage = Buffer.from(
    formatJson({
      schemaVersion: STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION,
      treasuryAddress,
      nodeEndpoint,
      reserveLovelace: reserveLovelace.toString(10),
      scope,
      entries,
    }) + "\n",
    "utf8",
  );
  const postimageSha256 = createHash("sha256").update(postimage).digest("hex");
  const timestamp = now().toISOString().replaceAll(":", "-");
  const evidenceId = timestamp + "-" + preimageSha256.slice(0, 16);
  const snapshotPath = join(
    outDir,
    "consolidation-state-v1-preimage-" + evidenceId + ".json",
  );
  const preparedAuditPath = join(
    outDir,
    "consolidation-upgrade-audit-" + evidenceId + "-prepared.json",
  );
  const auditPath = join(
    outDir,
    "consolidation-upgrade-audit-" + evidenceId + "-completed.json",
  );
  await assertFileGeneration(statePath, preimageSha256);
  for (const evidencePath of [snapshotPath, preparedAuditPath, auditPath]) {
    if (await fileExists(evidencePath)) {
      throw new Error(
        "Refusing to overwrite immutable consolidation upgrade evidence at " +
          evidencePath +
          ".",
      );
    }
  }
  const auditBase = {
    schemaVersion: "midgard-stress-wallet-consolidation-upgrade-v1" as const,
    statePath,
    snapshotPath,
    preparedAuditPath,
    auditPath,
    preimageSha256,
    postimageSha256,
    entryCount: entries.length,
    scope,
    treasuryAddress,
    nodeEndpoint,
    reserveLovelace: reserveLovelace.toString(10),
  };
  await writePrivateFileAtomicNoReplace(
    snapshotPath,
    preimage.toString("utf8"),
  );
  await writePrivateFileAtomicNoReplace(
    preparedAuditPath,
    formatJson({ ...auditBase, completed: false }) + "\n",
  );
  await assertFileGeneration(statePath, preimageSha256);
  await writePrivateFileAtomic(statePath, postimage.toString("utf8"));
  await assertFileGeneration(statePath, postimageSha256);
  await writePrivateFileAtomicNoReplace(
    auditPath,
    formatJson({ ...auditBase, completed: true }) + "\n",
  );
  return {
    schemaVersion: auditBase.schemaVersion,
    statePath,
    snapshotPath,
    preparedAuditPath,
    auditPath,
    preimageSha256,
    postimageSha256,
    entryCount: entries.length,
    scope,
  };
};

export const upgradeLegacyConsolidationState = async (
  options: UpgradeLegacyConsolidationStateOptions,
  runtime: UpgradeLegacyConsolidationStateRuntime,
): Promise<UpgradeLegacyConsolidationStateResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const statePath = join(outDir, "consolidation-state.json");
  return withExclusiveConsolidationStateLock(statePath, () =>
    upgradeLegacyConsolidationStateUnlocked(options, runtime),
  );
};

const waitForConsolidationAcceptance = async ({
  nodeEndpoint,
  txHash,
  runtime,
  acceptanceTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
}: {
  readonly nodeEndpoint: string;
  readonly txHash: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly acceptanceTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
}): Promise<string> => {
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const status = (await runtime.fetchTxStatus(nodeEndpoint, txHash))
      .trim()
      .toLowerCase();
    if (status === "committed") return status;
    if (rejectedTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} reached rejected status ${status}.`,
      );
    }
    if (!consolidationPendingTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} returned unknown status ${status || "<empty>"}; refusing to infer commitment/finality.`,
      );
    }
    if (monotonicNow() - startedAt >= acceptanceTimeoutMs) {
      throw new Error(
        `Timed out waiting ${acceptanceTimeoutMs.toString()}ms for consolidation transfer ${txHash}; last status ${status}.`,
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};

type ConsolidationReadinessSnapshot = {
  readonly httpStatus: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly durableAdmissionBacklog: number;
  readonly mempoolTxCount: number;
  readonly unfinishedLocalMutationJobs: number;
  readonly unresolvedBlockSubmissionAgeMs: number;
  readonly providerQueryHealthy: boolean;
  readonly leaseStatus: string;
  readonly pendingFinalizationCount: number;
  readonly commitWorkerActive: boolean;
  readonly commitPipelinePhase: string;
};

const requiredReadinessCount = (value: unknown, fieldName: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string" && /^\d+$/.test(value)
        ? Number(value)
        : Number.NaN;
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(
      "Malformed consolidation readiness response: " +
        fieldName +
        " must be a non-negative integer.",
    );
  }
  return parsed;
};

const parseConsolidationReadiness = (
  response: StressWalletConsolidationReadinessResponse,
): ConsolidationReadinessSnapshot => {
  if (!Number.isSafeInteger(response.httpStatus)) {
    throw new Error(
      "Malformed consolidation readiness response: httpStatus must be an integer.",
    );
  }
  const body = asObject(response.body, "consolidation readiness body");
  if (typeof body.ready !== "boolean") {
    throw new Error(
      "Malformed consolidation readiness response: ready must be boolean.",
    );
  }
  if (
    !Array.isArray(body.reasons) ||
    !body.reasons.every((reason) => typeof reason === "string")
  ) {
    throw new Error(
      "Malformed consolidation readiness response: reasons must be a string array.",
    );
  }
  if (typeof body.providerQueryHealthy !== "boolean") {
    throw new Error(
      "Malformed consolidation readiness response: providerQueryHealthy must be boolean.",
    );
  }
  const lease = asObject(
    body.stateQueueMutationLease,
    "stateQueueMutationLease",
  );
  const coordination = asObject(
    body.blockCommitmentCoordination,
    "blockCommitmentCoordination",
  );
  if (
    typeof lease.status !== "string" ||
    !Array.isArray(lease.pendingFinalizations)
  ) {
    throw new Error(
      "Malformed consolidation readiness response: lease status/pendingFinalizations are invalid.",
    );
  }
  if (
    typeof coordination.commitWorkerActive !== "boolean" ||
    typeof coordination.commitPipelinePhase !== "string"
  ) {
    throw new Error(
      "Malformed consolidation readiness response: commit coordination is invalid.",
    );
  }
  return {
    httpStatus: response.httpStatus,
    ready: body.ready,
    reasons: body.reasons,
    durableAdmissionBacklog: requiredReadinessCount(
      body.durableAdmissionBacklog,
      "durableAdmissionBacklog",
    ),
    mempoolTxCount: requiredReadinessCount(
      body.mempoolTxCount,
      "mempoolTxCount",
    ),
    unfinishedLocalMutationJobs: requiredReadinessCount(
      body.unfinishedLocalMutationJobs,
      "unfinishedLocalMutationJobs",
    ),
    unresolvedBlockSubmissionAgeMs: requiredReadinessCount(
      body.unresolvedBlockSubmissionAgeMs,
      "unresolvedBlockSubmissionAgeMs",
    ),
    providerQueryHealthy: body.providerQueryHealthy,
    leaseStatus: lease.status,
    pendingFinalizationCount: lease.pendingFinalizations.length,
    commitWorkerActive: coordination.commitWorkerActive,
    commitPipelinePhase: coordination.commitPipelinePhase,
  };
};

const defaultFetchConsolidationReadiness = async (
  nodeEndpoint: string,
  requestTimeoutMs: number,
): Promise<StressWalletConsolidationReadinessResponse> => {
  const response = await fetch(nodeEndpoint + "/readyz", {
    signal: AbortSignal.timeout(requestTimeoutMs),
  });
  const responseText = await response.text();
  let body: unknown;
  try {
    body = JSON.parse(responseText) as unknown;
  } catch {
    body = responseText;
  }
  return { httpStatus: response.status, body };
};

const isFullConsolidationReadiness = (
  snapshot: ConsolidationReadinessSnapshot,
): boolean =>
  snapshot.httpStatus === 200 &&
  snapshot.ready &&
  snapshot.reasons.length === 0 &&
  snapshot.durableAdmissionBacklog === 0 &&
  snapshot.mempoolTxCount === 0 &&
  snapshot.unfinishedLocalMutationJobs === 0 &&
  snapshot.unresolvedBlockSubmissionAgeMs === 0 &&
  snapshot.providerQueryHealthy &&
  snapshot.leaseStatus === "idle" &&
  snapshot.pendingFinalizationCount === 0 &&
  !snapshot.commitWorkerActive &&
  snapshot.commitPipelinePhase === "idle";

const waitForConsolidationReadiness = async ({
  nodeEndpoint,
  runtime,
  readinessPath,
  batchIndex,
  firstWalletId,
  timeoutMs,
  requestTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
  now,
}: {
  readonly nodeEndpoint: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly readinessPath: string;
  readonly batchIndex: number;
  readonly firstWalletId: string;
  readonly timeoutMs: number;
  readonly requestTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
  readonly now: () => Date;
}): Promise<void> => {
  const fetchReadiness =
    runtime.fetchReadiness ??
    ((endpoint: string) =>
      defaultFetchConsolidationReadiness(endpoint, requestTimeoutMs));
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const response = await fetchReadiness(nodeEndpoint);
    let snapshot: ConsolidationReadinessSnapshot;
    try {
      snapshot = parseConsolidationReadiness(response);
    } catch (error) {
      await writeFile(
        readinessPath,
        JSON.stringify({
          schemaVersion: "midgard-stress-wallet-consolidation-readiness-v1",
          observedAt: now().toISOString(),
          batchIndex,
          firstWalletId,
          attempt,
          malformed: true,
          error: error instanceof Error ? error.message : String(error),
          response,
        }) + "\n",
        { encoding: "utf8", flag: "a", mode: 0o600 },
      );
      throw error;
    }
    const fullReady = isFullConsolidationReadiness(snapshot);
    await writeFile(
      readinessPath,
      JSON.stringify({
        schemaVersion: "midgard-stress-wallet-consolidation-readiness-v1",
        observedAt: now().toISOString(),
        batchIndex,
        firstWalletId,
        attempt,
        fullReady,
        snapshot,
      }) + "\n",
      { encoding: "utf8", flag: "a", mode: 0o600 },
    );
    if (fullReady) return;
    if (monotonicNow() - startedAt >= timeoutMs) {
      throw new Error(
        "Timed out waiting " +
          timeoutMs.toString() +
          "ms for full consolidation readiness before batch " +
          batchIndex.toString() +
          " (first wallet " +
          firstWalletId +
          "); last snapshot " +
          formatJson(snapshot) +
          ".",
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};

const consolidateStressWalletsUnlocked = async (
  options: ConsolidateStressWalletsOptions,
  runtime: StressWalletConsolidateRuntime,
): Promise<StressWalletConsolidateResult> => {
  requireSafePositiveInteger(options.count, "count");
  const reserveLovelace =
    options.reserveLovelace ?? DEFAULT_CONSOLIDATE_RESERVE_LOVELACE;
  if (reserveLovelace < 0n) {
    throw new Error("reserveLovelace must be non-negative.");
  }
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(
    options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "acceptanceTimeoutMs",
  );
  const readinessTimeoutMs = requireSafeNonNegativeInteger(
    options.readinessTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "readinessTimeoutMs",
  );
  const verificationTimeoutMs = requireSafeNonNegativeInteger(
    options.verificationTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "verificationTimeoutMs",
  );
  const requestTimeoutMs = requireSafePositiveInteger(
    options.requestTimeoutMs ?? 30_000,
    "requestTimeoutMs",
  );
  const pollInitialIntervalMs = requireSafePositiveInteger(
    options.pollInitialIntervalMs ?? 250,
    "pollInitialIntervalMs",
  );
  const pollMaxIntervalMs = requireSafePositiveInteger(
    options.pollMaxIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS,
    "pollMaxIntervalMs",
  );
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const now = runtime.now ?? options.now ?? (() => new Date());
  const nodeEndpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const fetchUtxos =
    runtime.fetchUtxos ??
    ((endpoint: string, address: string) =>
      fetchNodeUtxosByAddress(endpoint, address, {
        timeoutMs: requestTimeoutMs,
      }));
  const sleepImpl = runtime.sleep ?? sleep;
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: false,
    now,
    generateSeedPhrase: defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const scope = buildStressWalletOperationScope({
    records,
    count: options.count,
    startIndex,
    envPrefix,
    network,
  });
  const treasuryAddress = deriveWalletInfo(
    { seedPhrase: options.treasurySeedPhrase, resolvedFrom: "treasury" },
    network,
  ).address;
  if (records.some((record) => record.l2Address === treasuryAddress)) {
    throw new Error(
      "Treasury address must be distinct from every stress wallet address.",
    );
  }

  const statePath = join(outDir, "consolidation-state.json");
  const readinessPath = join(outDir, "consolidation-readiness.jsonl");
  let expectedStateSha256 = await readFileGeneration(statePath);
  const priorState = await readConsolidationState(statePath);
  await assertFileGeneration(statePath, expectedStateSha256);
  if (
    priorState !== undefined &&
    (priorState.treasuryAddress !== treasuryAddress ||
      priorState.nodeEndpoint !== nodeEndpoint ||
      priorState.reserveLovelace !== reserveLovelace.toString(10) ||
      !sameStressWalletOperationScope(priorState.scope, scope))
  ) {
    throw new Error(
      `Existing consolidation state at ${statePath} does not match treasury, endpoint, reserve, or exact wallet scope.`,
    );
  }
  const recordsById = new Map(
    records.map((record) => [record.walletId, record]),
  );
  for (const entry of priorState?.entries ?? []) {
    const record = recordsById.get(entry.walletId);
    if (record === undefined || record.l2Address !== entry.address) {
      throw new Error(
        `Existing consolidation state at ${statePath} contains an entry outside the exact wallet scope.`,
      );
    }
    if (entry.signedTxCbor !== undefined) {
      assertConsolidationTransferIntent({
        entry,
        source: record,
        treasuryAddress,
        reserveLovelace,
        network,
      });
    }
  }
  const fetchSourceUtxos = async (): Promise<
    readonly (readonly NodeUtxo[])[]
  > => {
    const results: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(
      records.map((record, index) => ({ record, index })),
      maxInFlight,
      async ({ record, index }) => {
        results[index] = await fetchUtxos(nodeEndpoint, record.l2Address);
      },
    );
    return results;
  };

  // Complete the full read-only accounting pass before the first mutation.
  const treasuryBeforeUtxos = await fetchUtxos(nodeEndpoint, treasuryAddress);
  const sourceBeforeUtxos = await fetchSourceUtxos();
  const treasuryBefore = sumLovelace(treasuryBeforeUtxos);
  const sourceBefore = sourceBeforeUtxos.reduce(
    (total, utxos) => total + sumLovelace(utxos),
    0n,
  );
  const requestedByWallet = sourceBeforeUtxos.map((utxos) => {
    const total = sumLovelace(utxos);
    return total > reserveLovelace ? total - reserveLovelace : 0n;
  });
  const projectedTreasury = requestedByWallet.reduce(
    (total, amount) => total + amount,
    treasuryBefore,
  );
  if (
    options.requiredTreasuryLovelace !== undefined &&
    projectedTreasury < options.requiredTreasuryLovelace
  ) {
    throw new Error(
      `Projected treasury ${projectedTreasury.toString(10)} lovelace is below required ${options.requiredTreasuryLovelace.toString(10)} lovelace; no transfers submitted.`,
    );
  }

  const stateEntries = new Map(
    (priorState?.entries ?? []).map(
      (entry) => [entry.walletId, entry] as const,
    ),
  );
  let persistQueue = Promise.resolve();
  const persistState = async (): Promise<void> => {
    persistQueue = persistQueue.then(async () => {
      const entries = records.flatMap((record) => {
        const entry = stateEntries.get(record.walletId);
        return entry === undefined ? [] : [entry];
      });
      if (entries.length !== stateEntries.size) {
        throw new Error(
          "Refusing to persist consolidation state because an existing entry would be dropped.",
        );
      }
      const contents = `${formatJson({
        schemaVersion: STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION,
        treasuryAddress,
        nodeEndpoint,
        reserveLovelace: reserveLovelace.toString(10),
        scope,
        entries,
      })}\n`;
      await assertFileGeneration(statePath, expectedStateSha256);
      await writePrivateFileAtomic(statePath, contents);
      const writtenSha256 = sha256Bytes(Buffer.from(contents, "utf8"));
      await assertFileGeneration(statePath, writtenSha256);
      expectedStateSha256 = writtenSha256;
    });
    await persistQueue;
  };

  let submittedTransferCount = 0;
  let resumedTransferCount = 0;
  let alreadyConsolidatedCount = 0;
  const consolidationItems = records.map((record, index) => ({
    record,
    index,
  }));
  let batchIndex = 0;
  for (
    let batchStart = 0;
    batchStart < consolidationItems.length;
    batchStart += maxInFlight
  ) {
    const batch = consolidationItems.slice(
      batchStart,
      batchStart + maxInFlight,
    );
    const actionable = batch.filter(
      ({ index }) => requestedByWallet[index]! > 0n,
    );
    if (actionable.length === 0) {
      alreadyConsolidatedCount += batch.length;
      continue;
    }
    await waitForConsolidationReadiness({
      nodeEndpoint,
      runtime,
      readinessPath,
      batchIndex,
      firstWalletId: actionable[0]!.record.walletId,
      timeoutMs: readinessTimeoutMs,
      requestTimeoutMs,
      pollInitialIntervalMs,
      pollMaxIntervalMs,
      now,
    });
    await runBounded(batch, maxInFlight, async ({ record, index }) => {
      const requested = requestedByWallet[index]!;
      if (requested === 0n) {
        alreadyConsolidatedCount += 1;
        return;
      }
      const beforeUtxos = sourceBeforeUtxos[index]!;
      const beforeOutrefs = beforeUtxos.map(outRefKey).sort();
      let entry = stateEntries.get(record.walletId);
      if (entry !== undefined) {
        if (
          entry.address !== record.l2Address ||
          entry.beforeLovelace !== sumLovelace(beforeUtxos).toString(10) ||
          entry.requestedLovelace !== requested.toString(10) ||
          entry.beforeOutrefs.join("|") !== beforeOutrefs.join("|")
        ) {
          throw new Error(
            `Wallet ${record.walletId} changed after its consolidation intent was journaled; refusing an ambiguous resubmission.`,
          );
        }
        if (entry.signedTxCbor !== undefined) {
          const selectedInputs = entry.selectedInputs!;
          const beforeByOutref = new Map(
            beforeUtxos.map((utxo) => [outRefKey(utxo), utxo] as const),
          );
          const selectedUtxos = selectedInputs.map((input) =>
            beforeByOutref.get(input),
          );
          if (selectedUtxos.some((utxo) => utxo === undefined)) {
            throw new Error(
              `Wallet ${record.walletId} no longer contains every journaled selected input; refusing resubmission.`,
            );
          }
          const exactSelectedUtxos = selectedUtxos as readonly NodeUtxo[];
          if (
            exactSelectedUtxos.some((utxo) =>
              Object.entries(utxo.assets).some(
                ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
              ),
            ) ||
            sumLovelace(exactSelectedUtxos).toString(10) !==
              entry.selectedInputLovelace
          ) {
            throw new Error(
              `Wallet ${record.walletId} selected-input value does not match its exact journaled ADA-only snapshot; refusing resubmission.`,
            );
          }
          assertConsolidationTransferIntent({
            entry,
            source: record,
            treasuryAddress,
            reserveLovelace,
            network,
          });
        }
      } else {
        entry = {
          walletId: record.walletId,
          address: record.l2Address,
          beforeLovelace: sumLovelace(beforeUtxos).toString(10),
          beforeOutrefs,
          requestedLovelace: requested.toString(10),
        };
        stateEntries.set(record.walletId, entry);
        await persistState();
      }
      const hadPreparedTransaction = entry.txHash !== undefined;
      if (entry.txHash === undefined) {
        const prepared = await runtime.prepareTransfer({
          source: record,
          treasuryAddress,
          lovelace: requested,
        });
        if (!/^[0-9a-f]{64}$/.test(prepared.txHash)) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned an invalid tx hash.`,
          );
        }
        if (
          !/^[0-9a-f]+$/.test(prepared.signedTxCbor) ||
          prepared.signedTxCbor.length % 2 !== 0
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned invalid signed CBOR.`,
          );
        }
        if (
          computeSignedNativeTxHash(prepared.signedTxCbor, record.walletId) !==
          prepared.txHash
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned a tx hash that does not match its signed CBOR.`,
          );
        }
        if (
          prepared.selectedInputs.length === 0 ||
          new Set(prepared.selectedInputs).size !==
            prepared.selectedInputs.length ||
          prepared.selectedInputs.some(
            (input) => !beforeOutrefs.includes(input),
          )
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} selected inputs outside its journaled source snapshot.`,
          );
        }
        const beforeByOutref = new Map(
          beforeUtxos.map((utxo) => [outRefKey(utxo), utxo] as const),
        );
        const selectedUtxos = prepared.selectedInputs.map(
          (input) => beforeByOutref.get(input)!,
        );
        if (
          selectedUtxos.some((utxo) =>
            Object.entries(utxo.assets).some(
              ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
            ),
          )
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} selected a non-ADA source input.`,
          );
        }
        entry = {
          ...entry,
          txHash: prepared.txHash,
          signedTxCbor: prepared.signedTxCbor,
          selectedInputs: [...prepared.selectedInputs].sort(),
          selectedInputLovelace: sumLovelace(selectedUtxos).toString(10),
        };
        assertConsolidationTransferIntent({
          entry,
          source: record,
          treasuryAddress,
          reserveLovelace,
          network,
        });
        stateEntries.set(record.walletId, entry);
        // This durable checkpoint must complete before the first submit attempt.
        await persistState();
      } else {
        resumedTransferCount += 1;
      }
      const txHash = entry.txHash;
      if (txHash === undefined) {
        throw new Error(
          `Consolidation transfer for ${record.walletId} was not durably prepared.`,
        );
      }
      const observedStatus = (await runtime.fetchTxStatus(nodeEndpoint, txHash))
        .trim()
        .toLowerCase();
      if (observedStatus === "not_found") {
        if (entry.signedTxCbor === undefined) {
          throw new Error(
            `Committed legacy-upgrade checkpoint ${txHash} disappeared; exact signed CBOR is unavailable, so resubmission is forbidden.`,
          );
        }
        const submitted = await runtime.submitPreparedTransfer({
          nodeEndpoint,
          txHash,
          signedTxCbor: entry.signedTxCbor,
        });
        if (submitted.txHash !== txHash) {
          throw new Error(
            `Exact-CBOR submission returned tx hash ${submitted.txHash}, expected ${txHash}.`,
          );
        }
        submittedTransferCount += 1;
      } else if (rejectedTxStatuses.has(observedStatus)) {
        throw new Error(
          `Consolidation transfer ${txHash} reached rejected status ${observedStatus}.`,
        );
      } else if (
        observedStatus !== "committed" &&
        !consolidationPendingTxStatuses.has(observedStatus)
      ) {
        throw new Error(
          `Consolidation transfer ${txHash} returned unknown status ${observedStatus || "<empty>"}; refusing to resubmit or infer commitment.`,
        );
      }
      const acceptedStatus = await waitForConsolidationAcceptance({
        nodeEndpoint,
        txHash,
        runtime,
        acceptanceTimeoutMs,
        pollInitialIntervalMs,
        pollMaxIntervalMs,
      });
      stateEntries.set(record.walletId, {
        ...entry,
        txHash,
        acceptedStatus,
      });
      await persistState();
      if (!hadPreparedTransaction && entry.signedTxCbor === undefined) {
        throw new Error(
          `New consolidation entry for ${record.walletId} lost its signed transaction checkpoint.`,
        );
      }
    });
    batchIndex += 1;
  }

  // Reconcile every scoped journal entry to an explicit terminal state before
  // reporting completion, including wallets that were already at/below reserve.
  for (const { record, index } of consolidationItems) {
    const requested = requestedByWallet[index]!;
    const existing = stateEntries.get(record.walletId);
    if (requested === 0n) {
      if (existing === undefined) {
        const beforeUtxos = sourceBeforeUtxos[index]!;
        stateEntries.set(record.walletId, {
          walletId: record.walletId,
          address: record.l2Address,
          beforeLovelace: sumLovelace(beforeUtxos).toString(10),
          beforeOutrefs: beforeUtxos.map(outRefKey).sort(),
          requestedLovelace: "0",
          acceptedStatus: "already_empty",
        });
      } else if (existing.txHash !== undefined) {
        const status = (
          await runtime.fetchTxStatus(nodeEndpoint, existing.txHash)
        )
          .trim()
          .toLowerCase();
        if (status !== "committed") {
          throw new Error(
            `Terminal reconciliation found ${record.walletId} transaction ${existing.txHash} in status ${status || "<empty>"}.`,
          );
        }
        stateEntries.set(record.walletId, {
          ...existing,
          acceptedStatus: "committed",
        });
      } else if (
        existing.requestedLovelace !== "0" ||
        existing.acceptedStatus !== "already_empty"
      ) {
        throw new Error(
          `Terminal reconciliation found ambiguous entry for ${record.walletId}.`,
        );
      }
    } else if (existing?.acceptedStatus !== "committed") {
      throw new Error(
        `Terminal reconciliation found non-terminal entry for ${record.walletId}.`,
      );
    }
  }
  await persistState();

  const expectedTreasuryDelta = requestedByWallet.reduce(
    (total, amount) => total + amount,
    0n,
  );
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const verificationStartedAt = monotonicNow();
  let treasuryAfterUtxos: readonly NodeUtxo[] = [];
  let sourceAfterUtxos: readonly (readonly NodeUtxo[])[] = [];
  while (true) {
    treasuryAfterUtxos = await fetchUtxos(nodeEndpoint, treasuryAddress);
    sourceAfterUtxos = await fetchSourceUtxos();
    const treasuryDelta = sumLovelace(treasuryAfterUtxos) - treasuryBefore;
    const sourcesConsolidated = sourceAfterUtxos.every(
      (utxos) => sumLovelace(utxos) <= reserveLovelace,
    );
    if (treasuryDelta === expectedTreasuryDelta && sourcesConsolidated) break;
    if (monotonicNow() - verificationStartedAt >= verificationTimeoutMs) {
      throw new Error(
        `Consolidation accounting did not converge: treasury delta ${treasuryDelta.toString(10)} expected ${expectedTreasuryDelta.toString(10)}, sourcesConsolidated=${String(sourcesConsolidated)}.`,
      );
    }
    await sleepImpl(pollInitialIntervalMs);
  }

  const treasuryAfter = sumLovelace(treasuryAfterUtxos);
  const sourceAfter = sourceAfterUtxos.reduce(
    (total, utxos) => total + sumLovelace(utxos),
    0n,
  );
  const treasuryDelta = treasuryAfter - treasuryBefore;
  const inferredFees = sourceBefore - sourceAfter - treasuryDelta;
  if (inferredFees < 0n) {
    throw new Error(
      `Consolidation conservation failed with negative inferred fees ${inferredFees.toString(10)} lovelace.`,
    );
  }
  const reportTimestamp = now().toISOString();
  const reportPath = join(
    outDir,
    `consolidation-report-${reportTimestamp.replaceAll(":", "-")}.json`,
  );
  const result: StressWalletConsolidateResult = {
    schemaVersion: STRESS_WALLET_CONSOLIDATE_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    reserveLovelace: reserveLovelace.toString(10),
    maxInFlight,
    nodeEndpoint,
    treasuryAddress,
    treasuryBeforeLovelace: treasuryBefore.toString(10),
    treasuryAfterLovelace: treasuryAfter.toString(10),
    treasuryDeltaLovelace: treasuryDelta.toString(10),
    sourceBeforeLovelace: sourceBefore.toString(10),
    sourceAfterLovelace: sourceAfter.toString(10),
    inferredFeesLovelace: inferredFees.toString(10),
    projectedTreasuryLovelace: projectedTreasury.toString(10),
    submittedTransferCount,
    resumedTransferCount,
    alreadyConsolidatedCount,
    reportPath,
  };
  await writeFile(
    reportPath,
    `${formatJson({
      ...result,
      statePath,
      treasury: {
        before: utxoAccounting(treasuryBeforeUtxos),
        after: utxoAccounting(treasuryAfterUtxos),
      },
      wallets: records.map((record, index) => ({
        walletId: record.walletId,
        address: record.l2Address,
        before: utxoAccounting(sourceBeforeUtxos[index]!),
        after: utxoAccounting(sourceAfterUtxos[index]!),
        transfer: stateEntries.get(record.walletId),
      })),
    })}\n`,
    { encoding: "utf8", flag: "wx", mode: 0o600 },
  );
  return result;
};


type TerminalDrainEntry = {
  readonly walletId: string; readonly address: string;
  readonly beforeOutrefs: readonly string[]; readonly beforeLovelace: string;
  readonly beforeValueSha256: string; readonly status: "already_empty" | "prepared" | "committed";
  readonly txHash?: string; readonly signedTxCbor?: string; readonly selectedInputs?: readonly string[];
  readonly requestedLovelace?: string; readonly feeLovelace?: string; readonly signedTxBytes?: number;
};
type TerminalDrainState = {
  readonly schemaVersion: typeof STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION;
  readonly scope: StressWalletOperationScope; readonly scopeSha256: string;
  readonly nodeEndpoint: string; readonly network: Network; readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string; readonly minFeeA: string; readonly minFeeB: string;
  readonly feeCapLovelace: string; readonly maxFeeIterations: number;
  readonly entries: readonly TerminalDrainEntry[];
};
const terminalScopeHash = (scope: StressWalletOperationScope): string =>
  sha256Bytes(Buffer.from(JSON.stringify(scope), "utf8"));
const terminalSnapshotHash = (utxos: readonly NodeUtxo[]): string => sha256Bytes(Buffer.from(JSON.stringify(
  [...utxos].sort((a, b) => outRefKey(a).localeCompare(outRefKey(b))).map((utxo) => ({
    outref: outRefKey(utxo), outputCbor: utxo.outputCbor.toString("hex"),
    assets: Object.fromEntries(Object.entries(utxo.assets).filter(([, q]) => q !== 0n)
      .sort(([a], [b]) => a.localeCompare(b)).map(([unit, q]) => [unit, q.toString(10)])),
  }))), "utf8"));
const terminalDecimal = (value: unknown, label: string): string => {
  const parsed = requiredString(value, label);
  if (!/^(0|[1-9]\d*)$/.test(parsed)) throw new Error(label + " must be a canonical non-negative decimal.");
  return parsed;
};
const readTerminalDrainState = async (path: string): Promise<TerminalDrainState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  const raw = asObject(JSON.parse(await readFile(path, "utf8")) as unknown, "terminal drain state");
  if (raw.schemaVersion !== STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION || !Array.isArray(raw.entries))
    throw new Error("Unsupported terminal drain journal schema.");
  const scope = parseStressWalletOperationScope(raw.scope);
  const entries = raw.entries.map((value, index): TerminalDrainEntry => {
    const e = asObject(value, "entries[" + index.toString() + "]");
    if (!Array.isArray(e.beforeOutrefs) || (e.selectedInputs !== undefined && !Array.isArray(e.selectedInputs)))
      throw new Error("Terminal drain input lists must be arrays.");
    const status = requiredString(e.status, "status");
    if (status !== "already_empty" && status !== "prepared" && status !== "committed") throw new Error("Invalid terminal drain status.");
    if (status === "already_empty" && [e.txHash, e.signedTxCbor, e.selectedInputs, e.selectedInputLovelace,
      e.requestedLovelace, e.feeLovelace, e.signedTxBytes, e.acceptedStatus].some((field) => field !== undefined))
      throw new Error("Terminal drain already_empty entry contains forbidden transaction fields.");
    return {
      walletId: requiredString(e.walletId, "walletId"), address: requiredString(e.address, "address"),
      beforeOutrefs: e.beforeOutrefs.map((x) => requiredString(x, "beforeOutref")),
      beforeLovelace: terminalDecimal(e.beforeLovelace, "beforeLovelace"),
      beforeValueSha256: requiredString(e.beforeValueSha256, "beforeValueSha256"), status,
      ...(e.txHash === undefined ? {} : { txHash: requiredString(e.txHash, "txHash") }),
      ...(e.signedTxCbor === undefined ? {} : { signedTxCbor: requiredString(e.signedTxCbor, "signedTxCbor") }),
      ...(e.selectedInputs === undefined ? {} : { selectedInputs: e.selectedInputs.map((x) => requiredString(x, "selectedInput")) }),
      ...(e.requestedLovelace === undefined ? {} : { requestedLovelace: terminalDecimal(e.requestedLovelace, "requestedLovelace") }),
      ...(e.feeLovelace === undefined ? {} : { feeLovelace: terminalDecimal(e.feeLovelace, "feeLovelace") }),
      ...(e.signedTxBytes === undefined ? {} : { signedTxBytes: requiredPositiveInteger(e.signedTxBytes, "signedTxBytes") }),
    };
  });
  return {
    schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION, scope,
    scopeSha256: requiredString(raw.scopeSha256, "scopeSha256"),
    nodeEndpoint: requiredString(raw.nodeEndpoint, "nodeEndpoint"),
    network: parseStressWalletNetwork(requiredString(raw.network, "network"), {}),
    treasuryAddress: requiredString(raw.treasuryAddress, "treasuryAddress"),
    treasuryBeforeLovelace: terminalDecimal(raw.treasuryBeforeLovelace, "treasuryBeforeLovelace"),
    minFeeA: terminalDecimal(raw.minFeeA, "minFeeA"), minFeeB: terminalDecimal(raw.minFeeB, "minFeeB"),
    feeCapLovelace: terminalDecimal(raw.feeCapLovelace, "feeCapLovelace"),
    maxFeeIterations: requiredPositiveInteger(raw.maxFeeIterations, "maxFeeIterations"), entries,
  };
};
const assertTerminalDrainIntent = (entry: TerminalDrainEntry, source: StressWalletRecord, state: TerminalDrainState): void => {
  if (!/^[0-9a-f]{64}$/.test(entry.beforeValueSha256) || new Set(entry.beforeOutrefs).size !== entry.beforeOutrefs.length)
    throw new Error("Terminal drain snapshot digest/outrefs are malformed.");
  if (entry.status === "already_empty") {
    if (entry.beforeOutrefs.length !== 0 || entry.beforeLovelace !== "0" || entry.beforeValueSha256 !== terminalSnapshotHash([]) || entry.txHash !== undefined)
      throw new Error("Terminal drain already_empty entry is not exactly empty.");
    return;
  }
  if (!entry.txHash || !entry.signedTxCbor || !entry.selectedInputs || entry.requestedLovelace === undefined ||
      entry.feeLovelace === undefined || entry.signedTxBytes === undefined)
    throw new Error("Terminal drain prepared entry is incomplete.");
  if (!/^[0-9a-f]{64}$/.test(entry.txHash) || !/^[0-9a-f]+$/.test(entry.signedTxCbor) || entry.signedTxCbor.length % 2 !== 0)
    throw new Error("Terminal drain hash/CBOR encoding is malformed.");
  const bytes = Buffer.from(entry.signedTxCbor, "hex");
  const native = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
  const tx = decodeMidgardLedgerTxFromCanonicalCbor(bytes);
  if (tx.txId.toString("hex") !== entry.txHash || computeMidgardNativeTxId(native).toString("hex") !== entry.txHash)
    throw new Error("Terminal drain hash does not bind its exact signed CBOR.");
  if (tx.networkId !== (state.network === "Mainnet" ? 1n : 0n) || tx.validity !== "TxIsValid" ||
      native.version !== MIDGARD_NATIVE_TX_VERSION || native.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
      native.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE || tx.validityIntervalStart !== undefined || tx.validityIntervalEnd !== undefined)
    throw new Error("Terminal drain network/version/validity invariant failed.");
  const decodedInputs = tx.spendInputs.map((x) => x.txId.toString("hex") + "#" + x.index.toString()).sort();
  const selected = [...entry.selectedInputs].sort(); const before = [...entry.beforeOutrefs].sort();
  if (new Set(selected).size !== selected.length || decodedInputs.join("|") !== selected.join("|") || selected.join("|") !== before.join("|"))
    throw new Error("Terminal drain must spend every and only snapshotted input.");
  const total = BigInt(entry.beforeLovelace); const requested = BigInt(entry.requestedLovelace); const fee = BigInt(entry.feeLovelace);
  const requiredFee = BigInt(state.minFeeA) * BigInt(bytes.length) + BigInt(state.minFeeB);
  if (requested <= 0n || tx.fee !== fee || entry.signedTxBytes !== bytes.length || requested + fee !== total ||
      fee < requiredFee || fee > BigInt(state.feeCapLovelace)) throw new Error("Terminal drain fee/size/conservation invariant failed.");
  if (tx.outputs.length !== 1) throw new Error("Terminal drain must have exactly one output and zero source change.");
  const output = tx.outputs[0]!;
  if (encodeMidgardAddressText(output.address) !== state.treasuryAddress || output.value.lovelace !== requested ||
      output.value.assets.size !== 0 || output.datum !== undefined || output.scriptRef !== undefined)
    throw new Error("Terminal drain output is not the exact ADA-only treasury payment.");
  const signer = source.paymentKeyHash;
  if (tx.requiredSignerHashes.length !== 1 || tx.requiredSignerHashes[0]!.toString("hex") !== signer ||
      tx.witnessKeyHashes.length !== 1 || tx.witnessKeyHashes[0]!.toString("hex") !== signer || tx.vkeyWitnesses.length !== 1)
    throw new Error("Terminal drain is not signed solely by the source key.");
  const witness = tx.vkeyWitnesses[0]!; const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try { const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try { if (!publicKey.verify(tx.txId, signature)) throw new Error("Terminal drain signature is invalid."); }
    finally { signature.free(); }
  } finally { publicKey.free(); }
  if (tx.referenceInputs.length !== 0 || tx.requiredObserverHashes.length !== 0 || tx.scriptWitnesses.length !== 0 ||
      tx.nativeScriptHashes.length !== 0 || tx.plutusScriptHashes.length !== 0 || tx.redeemers.length !== 0 ||
      tx.mint.assets.length !== 0 || tx.requiresPlutusEvaluation || !tx.auxiliaryDataHash.equals(EMPTY_NULL_ROOT) ||
      !tx.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)) throw new Error("Terminal drain contains forbidden script/mint/reference/auxiliary content.");
};
const terminalDrainStressWalletsUnlocked = async (
  options: TerminalDrainStressWalletsOptions, runtime: StressWalletTerminalDrainRuntime,
): Promise<StressWalletTerminalDrainResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.minFeeA < 0n || options.minFeeB < 0n) throw new Error("Terminal drain fee parameters must be non-negative.");
  const feeCap = options.feeCapLovelace ?? 100_000n; if (feeCap < 0n) throw new Error("feeCapLovelace must be non-negative.");
  const maxFeeIterations = requireSafePositiveInteger(options.maxFeeIterations ?? 32, "maxFeeIterations");
  const maxInFlight = requireSafePositiveInteger(options.maxInFlight ?? DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT, "maxInFlight");
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS, "acceptanceTimeoutMs");
  const verificationTimeoutMs = requireSafeNonNegativeInteger(options.verificationTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS, "verificationTimeoutMs");
  const requestTimeoutMs = requireSafePositiveInteger(options.requestTimeoutMs ?? 30_000, "requestTimeoutMs");
  const pollInitial = requireSafePositiveInteger(options.pollInitialIntervalMs ?? 250, "pollInitialIntervalMs");
  const pollMax = requireSafePositiveInteger(options.pollMaxIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS, "pollMaxIntervalMs");
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR; const startIndex = requireSafePositiveInteger(options.startIndex ?? 1, "startIndex");
  const envPrefix = normalizeEnvPrefix(options.envPrefix); const network = options.network ?? "Preprod";
  const endpoint = defaultMidgardNodeEndpoint({ ...process.env, MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL });
  const now = runtime.now ?? options.now ?? (() => new Date()); const sleepImpl = runtime.sleep ?? sleep;
  const fetchUtxos = runtime.fetchUtxos ?? ((e: string, a: string) => fetchNodeUtxosByAddress(e, a, { timeoutMs: requestTimeoutMs }));
  const records = (await resolveStressWalletRecords({ count: options.count, outDir, startIndex, envPrefix, network,
    overwrite: false, reuseExisting: true, createMissing: false, now, generateSeedPhrase: defaultGenerateSeedPhrase })).map((x) => x.record);
  const scope = buildStressWalletOperationScope({ records, count: options.count, startIndex, envPrefix, network });
  const treasuryAddress = deriveWalletInfo({ seedPhrase: options.treasurySeedPhrase, resolvedFrom: "treasury" }, network).address;
  if (records.some((x) => x.l2Address === treasuryAddress)) throw new Error("Terminal drain treasury must differ from all sources.");
  const statePath = join(outDir, "terminal-drain-state.json"); let generation = await readFileGeneration(statePath);
  let state = await readTerminalDrainState(statePath); await assertFileGeneration(statePath, generation);
  const persist = async (next: TerminalDrainState): Promise<void> => { const text = formatJson(next) + "\n";
    await assertFileGeneration(statePath, generation); await writePrivateFileAtomic(statePath, text); await chmod(statePath, 0o600);
    generation = sha256Bytes(Buffer.from(text, "utf8")); await assertFileGeneration(statePath, generation); };
  if (state === undefined) {
    const treasuryBefore = sumLovelace(await fetchUtxos(endpoint, treasuryAddress));
    const snapshots: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(records.map((record, index) => ({ record, index })), maxInFlight, async ({ record, index }) => { snapshots[index] = await fetchUtxos(endpoint, record.l2Address); });
    const entries: TerminalDrainEntry[] = new Array(records.length);
    await runBounded(records.map((record, index) => ({ record, index })), maxInFlight, async ({ record, index }) => {
      const snapshot = snapshots[index]!; const beforeOutrefs = snapshot.map(outRefKey).sort(); const beforeLovelace = sumLovelace(snapshot);
      if (snapshot.some((u) => Object.entries(u.assets).some(([unit, q]) => unit !== "lovelace" && q !== 0n))) throw new Error("Terminal drain source contains non-ADA assets: " + record.walletId);
      if (snapshot.length === 0) { entries[index] = { walletId: record.walletId, address: record.l2Address, beforeOutrefs,
        beforeLovelace: "0", beforeValueSha256: terminalSnapshotHash(snapshot), status: "already_empty" }; return; }
      const p = await runtime.prepareTransfer({ source: record, treasuryAddress });
      entries[index] = { walletId: record.walletId, address: record.l2Address, beforeOutrefs,
        beforeLovelace: beforeLovelace.toString(10), beforeValueSha256: terminalSnapshotHash(snapshot), status: "prepared",
        txHash: p.txHash, signedTxCbor: p.signedTxCbor, selectedInputs: [...p.selectedInputs].sort(),
        requestedLovelace: p.requestedLovelace.toString(10), feeLovelace: p.feeLovelace.toString(10), signedTxBytes: p.signedTxBytes };
    });
    state = { schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION, scope, scopeSha256: terminalScopeHash(scope),
      nodeEndpoint: endpoint, network, treasuryAddress, treasuryBeforeLovelace: treasuryBefore.toString(10),
      minFeeA: options.minFeeA.toString(10), minFeeB: options.minFeeB.toString(10), feeCapLovelace: feeCap.toString(10), maxFeeIterations, entries };
    for (let i = 0; i < entries.length; i += 1) assertTerminalDrainIntent(entries[i]!, records[i]!, state);
    await persist(state);
  } else {
    if (!sameStressWalletOperationScope(state.scope, scope) || state.scopeSha256 !== terminalScopeHash(scope) || state.nodeEndpoint !== endpoint ||
        state.network !== network || state.treasuryAddress !== treasuryAddress || state.minFeeA !== options.minFeeA.toString(10) ||
        state.minFeeB !== options.minFeeB.toString(10) || state.feeCapLovelace !== feeCap.toString(10) || state.maxFeeIterations !== maxFeeIterations ||
        state.entries.length !== records.length) throw new Error("Terminal drain journal scope/config mismatch.");
    const ids = new Set<string>(); const hashes = new Set<string>();
    for (let i = 0; i < state.entries.length; i += 1) { const entry = state.entries[i]!; const record = records[i]!;
      if (entry.walletId !== record.walletId || entry.address !== record.l2Address || ids.has(entry.walletId) || (entry.txHash !== undefined && hashes.has(entry.txHash)))
        throw new Error("Terminal drain journal ordering/identity uniqueness mismatch.");
      ids.add(entry.walletId); if (entry.txHash) hashes.add(entry.txHash); assertTerminalDrainIntent(entry, record, state); }
  }
  const prepared = state.entries.filter((x) => x.status !== "already_empty").length;
  const alreadyEmpty = state.entries.length - prepared;
  const gross = state.entries.reduce((n, x) => n + BigInt(x.beforeLovelace), 0n);
  const fees = state.entries.reduce((n, x) => n + BigInt(x.feeLovelace ?? "0"), 0n);
  if (options.prepareOnly) return { schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION, phase: "prepared", walletDirectory: outDir,
    requestedCount: options.count, nodeEndpoint: endpoint, treasuryAddress, treasuryBeforeLovelace: state.treasuryBeforeLovelace,
    grossSourceLovelace: gross.toString(10), totalFeesLovelace: fees.toString(10), preparedTransferCount: prepared,
    alreadyEmptyCount: alreadyEmpty, submittedTransferCount: 0, resumedTransferCount: 0, statePath };
  let submitted = 0; let resumed = 0; const entries = [...state.entries];
  for (let i = 0; i < entries.length; i += 1) { let entry = entries[i]!; if (entry.status === "already_empty") continue;
    let status = (await runtime.fetchTxStatus(endpoint, entry.txHash!)).trim().toLowerCase();
    if (entry.status === "committed") { if (status !== "committed") throw new Error("Committed terminal drain checkpoint lost finality."); resumed += 1; continue; }
    if (status === "not_found") { const current = await fetchUtxos(endpoint, entry.address);
      if (current.map(outRefKey).sort().join("|") !== entry.beforeOutrefs.join("|") || terminalSnapshotHash(current) !== entry.beforeValueSha256)
        throw new Error("Terminal drain not_found transaction has missing/changed inputs; exact-CBOR resubmission refused.");
      const result = await runtime.submitPreparedTransfer({ nodeEndpoint: endpoint, txHash: entry.txHash!, signedTxCbor: entry.signedTxCbor! });
      if (result.txHash !== entry.txHash) throw new Error("Terminal drain submit returned mismatched hash."); submitted += 1;
    } else if (status !== "committed" && !consolidationPendingTxStatuses.has(status)) throw new Error("Terminal drain status is not committable: " + status);
    else resumed += 1;
    const started = (runtime.monotonicNow ?? (() => Date.now()))(); let attempt = 0;
    while ((status = (await runtime.fetchTxStatus(endpoint, entry.txHash!)).trim().toLowerCase()) !== "committed") {
      if (rejectedTxStatuses.has(status) || !consolidationPendingTxStatuses.has(status)) throw new Error("Terminal drain reached non-committable status " + status);
      if ((runtime.monotonicNow ?? (() => Date.now()))() - started >= acceptanceTimeoutMs) throw new Error("Timed out waiting for terminal drain commitment.");
      await sleepImpl(nextFanoutPollDelayMs({ attempt, initialMs: pollInitial, maxMs: pollMax })); attempt += 1;
    }
    entry = { ...entry, status: "committed" }; entries[i] = entry; state = { ...state, entries }; await persist(state);
  }
  const verifyStart = (runtime.monotonicNow ?? (() => Date.now()))(); let treasuryAfter = 0n; let sources: readonly (readonly NodeUtxo[])[] = [];
  while (true) { treasuryAfter = sumLovelace(await fetchUtxos(endpoint, treasuryAddress)); const next: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(records.map((record, index) => ({ record, index })), maxInFlight, async ({ record, index }) => { next[index] = await fetchUtxos(endpoint, record.l2Address); }); sources = next;
    const residual = sources.reduce((n, x) => n + sumLovelace(x), 0n); const delta = treasuryAfter - BigInt(state.treasuryBeforeLovelace);
    if (residual === 0n && sources.every((utxos) => utxos.length === 0) && delta + fees === gross) break;
    if ((runtime.monotonicNow ?? (() => Date.now()))() - verifyStart >= verificationTimeoutMs) throw new Error("Terminal drain exact-zero conservation did not converge.");
    await sleepImpl(pollInitial);
  }
  for (const entry of state.entries) if (entry.status !== "already_empty" && (await runtime.fetchTxStatus(endpoint, entry.txHash!)).trim().toLowerCase() !== "committed")
    throw new Error("Terminal drain final status reconciliation failed.");
  const delta = treasuryAfter - BigInt(state.treasuryBeforeLovelace); const reportPath = join(outDir, "terminal-drain-report-" + now().toISOString().replaceAll(":", "-") + ".json");
  await writePrivateFileAtomicNoReplace(reportPath, formatJson({ schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION, scope, statePath, endpoint, network, treasuryAddress,
    conservation: { grossSourceLovelace: gross.toString(10), treasuryDeltaLovelace: delta.toString(10), totalFeesLovelace: fees.toString(10), residualSourceLovelace: "0" },
    wallets: records.map((record, i) => ({ walletId: record.walletId, address: record.l2Address, before: state!.entries[i], after: utxoAccounting(sources[i]!) })) }) + "\n");
  return { schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_SCHEMA_VERSION, phase: "committed", walletDirectory: outDir, requestedCount: options.count,
    nodeEndpoint: endpoint, treasuryAddress, treasuryBeforeLovelace: state.treasuryBeforeLovelace, treasuryAfterLovelace: treasuryAfter.toString(10),
    treasuryDeltaLovelace: delta.toString(10), grossSourceLovelace: gross.toString(10), totalFeesLovelace: fees.toString(10), residualSourceLovelace: "0",
    preparedTransferCount: prepared, alreadyEmptyCount: alreadyEmpty, submittedTransferCount: submitted, resumedTransferCount: resumed, statePath, reportPath };
};
export const terminalDrainStressWallets = async (options: TerminalDrainStressWalletsOptions, runtime: StressWalletTerminalDrainRuntime): Promise<StressWalletTerminalDrainResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () => terminalDrainStressWalletsUnlocked(options, runtime));
};

export const consolidateStressWallets = async (
  options: ConsolidateStressWalletsOptions,
  runtime: StressWalletConsolidateRuntime,
): Promise<StressWalletConsolidateResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const statePath = join(outDir, "consolidation-state.json");
  return withExclusiveStressWalletFundsLock(outDir, () =>
    withExclusiveConsolidationStateLock(statePath, () =>
      consolidateStressWalletsUnlocked(options, runtime),
    ),
  );
};
