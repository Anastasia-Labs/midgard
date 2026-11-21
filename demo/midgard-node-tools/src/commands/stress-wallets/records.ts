import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { type Network } from "@lucid-evolution/lucid";
import {
  deriveWalletInfo,
  formatJson,
} from "midgard-node/commands/command-utils";

import {
  asObject,
  assertExactKeys,
  requiredPositiveInteger,
  requiredString,
} from "./artifact-fields.js";
import { parseStressWalletCreateResult } from "./artifacts.js";
import {
  DEFAULT_STRESS_WALLET_DIR,
  ENV_NAME_PATTERN,
  STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_RECORD_SCHEMA_VERSION,
} from "./constants.js";
import { fileExists, writePrivateFileAtomic } from "./files.js";
import {
  defaultGenerateSeedPhrase,
  normalizeEnvPrefix,
  normalizeSeedPhrase,
  parseStressWalletNetwork,
  requireSafePositiveInteger,
  stressWalletEnvName,
  stressWalletId,
  walletIndexLabel,
  walletPath,
} from "./options.js";
import {
  type CreateL2WalletsOptions,
  type CreateL2WalletsResult,
  type ResolvedStressWallet,
  type StressWalletExportArtifacts,
  type StressWalletRecord,
  type StressWalletSummary,
} from "./types.js";
import {
  parseLatestFunding,
  parseStressWalletSummaryArtifact,
} from "./wallet-summary.js";

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
    schemaVersion: STRESS_WALLET_RECORD_SCHEMA_VERSION,
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

export const parseStressWalletRecord = (value: unknown): StressWalletRecord => {
  const raw = asObject(value, "stress wallet record");
  assertExactKeys(
    raw,
    "stress wallet record",
    [
      "schemaVersion",
      "walletId",
      "index",
      "envName",
      "network",
      "seedPhrase",
      "l2Address",
      "paymentKeyHash",
      "createdAt",
    ],
    ["latestFunding"],
  );
  const schemaVersion = requiredString(raw.schemaVersion, "schemaVersion");
  if (schemaVersion !== STRESS_WALLET_RECORD_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress wallet schemaVersion "${schemaVersion}".`,
    );
  }
  const network = parseStressWalletNetwork(
    requiredString(raw.network, "network"),
    {},
  );
  const seedPhrase = requiredString(raw.seedPhrase, "seedPhrase");
  if (normalizeSeedPhrase(seedPhrase) !== seedPhrase) {
    throw new Error("seedPhrase must use canonical single-space word framing.");
  }
  const record: StressWalletRecord = {
    schemaVersion: STRESS_WALLET_RECORD_SCHEMA_VERSION,
    walletId: requiredString(raw.walletId, "walletId"),
    index: requiredPositiveInteger(raw.index, "index"),
    envName: requiredString(raw.envName, "envName"),
    network,
    seedPhrase,
    l2Address: requiredString(raw.l2Address, "l2Address"),
    paymentKeyHash: requiredString(raw.paymentKeyHash, "paymentKeyHash"),
    createdAt: requiredString(raw.createdAt, "createdAt"),
    latestFunding: parseLatestFunding(raw.latestFunding),
  };
  parseStressWalletSummaryArtifact(
    {
      schemaVersion: record.schemaVersion,
      walletId: record.walletId,
      index: record.index,
      envName: record.envName,
      network: record.network,
      l2Address: record.l2Address,
      paymentKeyHash: record.paymentKeyHash,
      createdAt: record.createdAt,
      ...(record.latestFunding === undefined
        ? {}
        : { latestFunding: record.latestFunding }),
      path: "<memory>",
    },
    "stress wallet record",
  );
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

export const writeStressWalletRecord = async (
  path: string,
  record: StressWalletRecord,
): Promise<void> => {
  const canonical = parseStressWalletRecord(
    JSON.parse(formatJson(record)) as unknown,
  );
  await writePrivateFileAtomic(path, `${formatJson(canonical)}\n`);
};

export const summaryForRecord = (
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

export const writeStressWalletExports = async (
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

export const resolveStressWalletRecords = async ({
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
  const result: CreateL2WalletsResult = {
    schemaVersion: STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
    walletDirectory: outDir,
    createdCount: resolved.filter((wallet) => wallet.created).length,
    reusedCount: resolved.filter((wallet) => !wallet.created).length,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    wallets: resolved.map(({ path, record }) => summaryForRecord(record, path)),
  };
  parseStressWalletCreateResult(result);
  return result;
};
