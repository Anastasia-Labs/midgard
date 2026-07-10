import { access, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { type Network } from "@lucid-evolution/lucid";
import { generateMnemonic } from "bip39";

import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
} from "@/commands/command-utils.js";
import { writeTextFileAtomic } from "@/files/atomic-write.js";

export const STRESS_WALLET_SCHEMA_VERSION = "midgard-stress-wallet-v1";
export const STRESS_WALLET_PREPARE_SCHEMA_VERSION =
  "midgard-stress-wallet-prepare-v1";
export const DEFAULT_STRESS_WALLET_DIR = ".stress-wallets";
export const DEFAULT_STRESS_WALLET_ENV_PREFIX = "STRESS_WALLET_SEED_PHRASE";
export const DEFAULT_PROJECTION_WAIT_MS = 120_000;
export const DEFAULT_VERIFY_TIMEOUT_MS = 300_000;
export const DEFAULT_VERIFY_POLL_INTERVAL_MS = 5_000;

const ENV_NAME_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

export type StressWalletFundingSnapshot = {
  readonly preparedAt: string;
  readonly status: "submitted" | "already_funded";
  readonly lovelacePerWallet: string;
  readonly nodeEndpoint: string;
  readonly beforeUtxoCount: number;
  readonly afterUtxoCount: number;
  readonly verifiedFundingUtxoCount: number;
  readonly depositTxHash?: string;
  readonly depositEventId?: string;
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

const writePrivateFileAtomic = async (
  path: string,
  contents: string,
): Promise<void> => writeTextFileAtomic(path, contents, { mode: 0o600 });

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
  expectedEnvName,
  expectedNetwork,
}: {
  readonly record: StressWalletRecord;
  readonly path: string;
  readonly expectedIndex: number;
  readonly expectedEnvName: string;
  readonly expectedNetwork: Network;
}): void => {
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
  const envNames = new Set<string>();
  const addresses = new Set<string>();
  for (const record of records) {
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
    envNames.add(record.envName);
    addresses.add(record.l2Address);
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

export const prepareStressWallets = async (
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
