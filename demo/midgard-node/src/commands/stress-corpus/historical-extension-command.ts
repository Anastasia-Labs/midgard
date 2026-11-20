import { createHash } from "node:crypto";
import { constants as fsConstants, createReadStream } from "node:fs";
import {
  chmod,
  copyFile,
  mkdir,
  mkdtemp,
  open,
  readdir,
  readFile,
  rename,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { cpus } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { Worker } from "node:worker_threads";

import type { Network } from "@lucid-evolution/lucid";

import { formatJson } from "@/commands/command-utils.js";
import {
  type AssembleCorpusResult,
  assembleCorpusShards,
} from "@/commands/stress-corpus/assemble.js";
import type { CorpusFundingUtxo } from "@/commands/stress-corpus/build-chain.js";
import { nodeUtxoFromCorpusFunding } from "@/commands/stress-corpus/build-chain.js";
import {
  assertHistoricalExtensionSchedule,
  createHistoricalExtensionSchedule,
  type HistoricalExtensionCorpusVerification,
  type HistoricalExtensionSchedule,
  parseCorpusIndexEntries,
  readVerifiedHistoricalRetainedTerminals,
  type VerifiedHistoricalRetainedTerminal,
  verifyHistoricalExtensionCorpus,
} from "@/commands/stress-corpus/historical-extension.js";
import {
  computeStressCorpusWalletSetIdentity,
  type StressCorpusWalletSetIdentity,
} from "@/commands/stress-corpus/wallet-set-identity.js";
import {
  parseStressWalletRecord,
  type StressWalletRecord,
} from "@/commands/stress-wallets.js";
import { resolveWorkerEntry } from "@/fibers/resolve-worker-entry.js";
import {
  type CorpusWorkerInput,
  type CorpusWorkerOutput,
  type CorpusWorkerWallet,
  runCorpusChainWorker,
} from "@/workers/corpus-chain-builder.js";

export const HISTORICAL_EXTENSION_MANIFEST_SCHEMA_VERSION =
  "midgard-stress-corpus-historical-extension-manifest-v1";
export const HISTORICAL_EXTENSION_VERIFICATION_SCHEMA_VERSION =
  "midgard-stress-corpus-historical-extension-verification-v1";
export const HISTORICAL_EXTENSION_BINDING_SCHEMA_VERSION =
  "midgard-phase5-historical-corpus-binding-v1";
export const HISTORICAL_EXTENSION_GENERATION_SCHEMA_VERSION =
  "midgard-stress-corpus-historical-extension-generation-v1";
export const HISTORICAL_EXTENSION_CLAIM_SCOPE =
  "historical-offline-corpus-extension";
export const HISTORICAL_EXTENSION_COMPATIBILITY = {
  consumerScope: "phase5-da-distribution-only",
  chainRunLayout: "retained-base-runs-then-continuation-runs",
  phase1FormalBindingCompatible: false,
  phase2ValidationCorpusCompatible: false,
} as const;
export const DEFAULT_HISTORICAL_EXTENSION_BASE_CHAIN_COUNT = 4_096;
export const DEFAULT_HISTORICAL_EXTENSION_BASE_DEPTH = 748;
export const DEFAULT_HISTORICAL_EXTENSION_TARGET_ROW_COUNT = 5_000_000;
export const HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM =
  "sha256-chain-id-outref-output-cbor-sha256-lovelace-lines-v1";

const BASE_MANIFEST_SCHEMA_VERSION = "midgard-stress-corpus-manifest-v1";
const BASE_VERIFICATION_SCHEMA_VERSION =
  "midgard-stress-corpus-verification-v1";
const BASE_BINDING_SCHEMA_VERSION = "midgard-phase1-live-corpus-binding-v2";
const FANOUT_SCHEMA_VERSION = "midgard-stress-wallet-fanout-v1";
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

export type ImmutableFileIdentity = {
  readonly path: string;
  readonly sha256: string;
};

export type HistoricalExtensionBaseEvidence = {
  readonly corpus: ImmutableFileIdentity;
  readonly index: ImmutableFileIdentity;
  readonly manifest: ImmutableFileIdentity;
  readonly verification: ImmutableFileIdentity;
  readonly phase1Binding: ImmutableFileIdentity & {
    readonly schemaVersion: typeof BASE_BINDING_SCHEMA_VERSION;
  };
  readonly fanoutReport: ImmutableFileIdentity & {
    readonly schemaVersion: typeof FANOUT_SCHEMA_VERSION;
  };
};

export type HistoricalExtensionConfig = {
  readonly baseCorpus: ImmutableFileIdentity;
  readonly baseIndex: ImmutableFileIdentity;
  readonly baseManifest: ImmutableFileIdentity;
  readonly baseVerification: ImmutableFileIdentity;
  readonly baseBinding: ImmutableFileIdentity;
  readonly fanoutReport: ImmutableFileIdentity;
  readonly walletsDir: string;
  readonly outDir: string;
  readonly baseChainCount: number;
  readonly baseDepth: number;
  readonly targetRowCount: number;
  readonly workers: number;
  readonly yes: boolean;
};

export type HistoricalExtensionRuntime = {
  /** Test/operator hook that runs only after every mutable input is snapshotted. */
  readonly afterImmutableSnapshot?: () => Promise<void>;
  /** Test hook immediately before the complete staged directory is promoted. */
  readonly beforeAtomicPromotion?: () => Promise<void>;
};

export type VerifiedHistoricalExtensionBase = {
  readonly evidence: HistoricalExtensionBaseEvidence;
  readonly walletSetIdentity: StressCorpusWalletSetIdentity;
  readonly walletRecords: readonly StressWalletRecord[];
  readonly schedule: HistoricalExtensionSchedule;
  readonly retainedTerminals: readonly VerifiedHistoricalRetainedTerminal[];
  readonly retainedTerminalSetSha256: string;
  readonly build: {
    readonly amountLovelace: bigint;
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
    readonly network: Network;
    readonly networkId: bigint;
    readonly maxSubmitTxCborBytes: number;
    readonly corpusSliceId: string;
    readonly planShape: "chain";
  };
};

export type HistoricalExtensionGenerationResult = {
  readonly schemaVersion: typeof HISTORICAL_EXTENSION_GENERATION_SCHEMA_VERSION;
  readonly claimScope: typeof HISTORICAL_EXTENSION_CLAIM_SCOPE;
  readonly freshLiveClaim: false;
  readonly compatibility: typeof HISTORICAL_EXTENSION_COMPATIBILITY;
  readonly generatedAtIso: string;
  readonly outDir: string;
  readonly baseEvidence: HistoricalExtensionBaseEvidence;
  readonly walletSetIdentity: StressCorpusWalletSetIdentity;
  readonly schedule: HistoricalExtensionSchedule;
  readonly fundingModel: HistoricalExtensionFundingModel;
  readonly files: {
    readonly corpus: ImmutableFileIdentity & { readonly rowCount: number };
    readonly index: ImmutableFileIdentity & { readonly rowCount: number };
    readonly manifest: ImmutableFileIdentity;
    readonly verification: ImmutableFileIdentity;
    readonly historicalBinding: ImmutableFileIdentity;
  };
  readonly assembled: {
    readonly rowCount: number;
    readonly indexEntryCount: number;
    readonly corpusSha256: string;
    readonly indexSha256: string;
  };
  readonly verification: HistoricalExtensionCorpusVerification;
  readonly generationResultPath: string;
};

export type HistoricalExtensionFundingModel = {
  readonly source: "cryptographically-verified-retained-terminal-output-1-per-wallet";
  readonly retainedBaseOriginalFundingSetSha256: string;
  readonly retainedTerminalSetHashAlgorithm: typeof HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM;
  readonly retainedTerminalSetSha256: string;
  readonly freshFundingLovelace: "0";
  readonly retainedTerminalLovelaceTotal: string;
  readonly continuationFundingValueSource: "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain";
  readonly amountLovelacePerRow: string;
  readonly retainedBaseRequestedTransferLovelace: string;
  readonly extensionRequestedTransferLovelace: string;
  readonly feeFormula: {
    readonly minFeeA: string;
    readonly minFeeB: string;
    readonly formula: "minFeeA * canonicalCborByteLength + minFeeB";
  };
  readonly minimumTerminalChangeLovelacePerChain: string;
  readonly proof: "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows";
};

type JsonObject = Record<string, unknown>;

const requireObject = (value: unknown, label: string): JsonObject => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object.`);
  }
  return value as JsonObject;
};

const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty string.`);
  }
  return value.trim();
};

const requireSha256 = (value: unknown, label: string): string => {
  const normalized = requireString(value, label).toLowerCase();
  if (!SHA256_PATTERN.test(normalized)) {
    throw new Error(`${label} must be a lowercase SHA-256 digest.`);
  }
  return normalized;
};

const positiveSafeInteger = (value: unknown, label: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string"
        ? Number(value)
        : NaN;
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
  return parsed;
};

const nonNegativeBigInt = (value: unknown, label: string): bigint => {
  const text = String(value);
  if (!/^(0|[1-9][0-9]*)$/u.test(text)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(text);
};

const positiveBigInt = (value: unknown, label: string): bigint => {
  const parsed = nonNegativeBigInt(value, label);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

const fileIdentityFromInput = (
  input: Record<string, unknown>,
  pathField: string,
  hashField: string,
): ImmutableFileIdentity => ({
  path: resolve(requireString(input[pathField], `--${pathField}`)),
  sha256: requireSha256(input[hashField], `--${hashField}`),
});

export const parseHistoricalExtensionConfig = (
  input: Record<string, unknown>,
): HistoricalExtensionConfig => ({
  baseCorpus: fileIdentityFromInput(
    input,
    "baseCorpusPath",
    "baseCorpusSha256",
  ),
  baseIndex: fileIdentityFromInput(input, "baseIndexPath", "baseIndexSha256"),
  baseManifest: fileIdentityFromInput(
    input,
    "baseManifestPath",
    "baseManifestSha256",
  ),
  baseVerification: fileIdentityFromInput(
    input,
    "baseVerificationPath",
    "baseVerificationSha256",
  ),
  baseBinding: fileIdentityFromInput(
    input,
    "baseBindingPath",
    "baseBindingSha256",
  ),
  fanoutReport: fileIdentityFromInput(
    input,
    "fanoutReportPath",
    "fanoutReportSha256",
  ),
  walletsDir: resolve(requireString(input.walletsDir, "--walletsDir")),
  outDir: resolve(requireString(input.outDir, "--outDir")),
  baseChainCount: positiveSafeInteger(
    input.baseChainCount ?? DEFAULT_HISTORICAL_EXTENSION_BASE_CHAIN_COUNT,
    "--baseChainCount",
  ),
  baseDepth: positiveSafeInteger(
    input.baseDepth ?? DEFAULT_HISTORICAL_EXTENSION_BASE_DEPTH,
    "--baseDepth",
  ),
  targetRowCount: positiveSafeInteger(
    input.targetRowCount ?? DEFAULT_HISTORICAL_EXTENSION_TARGET_ROW_COUNT,
    "--targetRowCount",
  ),
  workers: positiveSafeInteger(
    input.workers ?? Math.max(1, cpus().length - 1),
    "--workers",
  ),
  yes: input.yes === true,
});

const sha256File = async (path: string): Promise<string> =>
  new Promise((resolveHash, reject) => {
    const hash = createHash("sha256");
    const input = createReadStream(path);
    input.on("data", (chunk: string | Buffer) => hash.update(chunk));
    input.on("error", reject);
    input.on("end", () => resolveHash(hash.digest("hex")));
  });

const readJson = async (path: string, label: string): Promise<JsonObject> => {
  try {
    return requireObject(JSON.parse(await readFile(path, "utf8")), label);
  } catch (error) {
    throw new Error(
      `Unable to read ${label} ${path}: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

const assertFileIdentity = async (
  identity: ImmutableFileIdentity,
  label: string,
): Promise<void> => {
  const actual = await sha256File(identity.path);
  if (actual !== identity.sha256) {
    throw new Error(
      `${label} SHA-256 mismatch: expected ${identity.sha256}, received ${actual}.`,
    );
  }
};

const publishedBaseEvidence = (
  config: HistoricalExtensionConfig,
): HistoricalExtensionBaseEvidence => ({
  corpus: config.baseCorpus,
  index: config.baseIndex,
  manifest: config.baseManifest,
  verification: config.baseVerification,
  phase1Binding: {
    ...config.baseBinding,
    schemaVersion: BASE_BINDING_SCHEMA_VERSION,
  },
  fanoutReport: {
    ...config.fanoutReport,
    schemaVersion: FANOUT_SCHEMA_VERSION,
  },
});

const copyImmutableInput = async ({
  source,
  targetPath,
  label,
}: {
  readonly source: ImmutableFileIdentity;
  readonly targetPath: string;
  readonly label: string;
}): Promise<ImmutableFileIdentity> => {
  await copyFile(source.path, targetPath, fsConstants.COPYFILE_FICLONE);
  await chmod(targetPath, 0o400);
  const snapshot = { path: targetPath, sha256: source.sha256 };
  await assertFileIdentity(snapshot, `${label} immutable snapshot`);
  return snapshot;
};

const createImmutableInputSnapshot = async ({
  config,
  stageRoot,
}: {
  readonly config: HistoricalExtensionConfig;
  readonly stageRoot: string;
}): Promise<HistoricalExtensionConfig> => {
  const snapshotDir = join(stageRoot, "immutable-inputs");
  await mkdir(snapshotDir, { recursive: true, mode: 0o700 });
  const [
    baseCorpus,
    baseIndex,
    baseManifest,
    baseVerification,
    baseBinding,
    fanoutReport,
  ] = await Promise.all([
    copyImmutableInput({
      source: config.baseCorpus,
      targetPath: join(snapshotDir, "base-corpus.ndjson"),
      label: "base corpus",
    }),
    copyImmutableInput({
      source: config.baseIndex,
      targetPath: join(snapshotDir, "base-index.ndjson"),
      label: "base index",
    }),
    copyImmutableInput({
      source: config.baseManifest,
      targetPath: join(snapshotDir, "base-manifest.json"),
      label: "base manifest",
    }),
    copyImmutableInput({
      source: config.baseVerification,
      targetPath: join(snapshotDir, "base-verification.json"),
      label: "base verification",
    }),
    copyImmutableInput({
      source: config.baseBinding,
      targetPath: join(snapshotDir, "base-binding.json"),
      label: "base binding",
    }),
    copyImmutableInput({
      source: config.fanoutReport,
      targetPath: join(snapshotDir, "fanout-report.json"),
      label: "fanout report",
    }),
  ]);
  await chmod(snapshotDir, 0o500);
  return {
    ...config,
    baseCorpus,
    baseIndex,
    baseManifest,
    baseVerification,
    baseBinding,
    fanoutReport,
    outDir: join(stageRoot, "publish"),
  };
};

const walletFilePattern = /^wallet-\d{4}\.json$/u;

const readWalletRecords = async (
  walletsDir: string,
  expectedCount: number,
): Promise<readonly StressWalletRecord[]> => {
  const files = (await readdir(walletsDir))
    .filter((file) => walletFilePattern.test(file))
    .sort();
  if (files.length !== expectedCount) {
    throw new Error(
      `wallets directory has ${files.length.toString()} records, expected exactly ${expectedCount.toString()}.`,
    );
  }
  return Promise.all(
    files.map(async (file) =>
      parseStressWalletRecord(
        JSON.parse(await readFile(join(walletsDir, file), "utf8")) as unknown,
      ),
    ),
  );
};

const fundingUtxoForRecord = (
  record: StressWalletRecord,
): CorpusFundingUtxo => {
  const funding = record.latestFunding?.fundingUtxos?.[0];
  if (funding === undefined) {
    throw new Error(
      `Stress wallet ${record.walletId} has no first funding UTxO.`,
    );
  }
  const [txHash, indexRaw, extra] = funding.outref
    .trim()
    .toLowerCase()
    .split("#");
  if (
    txHash === undefined ||
    indexRaw === undefined ||
    extra !== undefined ||
    !/^[0-9a-f]{64}$/u.test(txHash) ||
    !/^(0|[1-9][0-9]*)$/u.test(indexRaw)
  ) {
    throw new Error(
      `Stress wallet ${record.walletId} first funding outref is invalid.`,
    );
  }
  return {
    txHash,
    outputIndex: Number(indexRaw),
    outputCborHex: funding.outputCbor,
  };
};

const firstFundingLovelaceFromBoundOutput = (
  record: StressWalletRecord,
): bigint => {
  const funding = record.latestFunding?.fundingUtxos?.[0];
  if (funding === undefined) {
    throw new Error(
      `Stress wallet ${record.walletId} has no first funding UTxO.`,
    );
  }
  const derived =
    nodeUtxoFromCorpusFunding(fundingUtxoForRecord(record)).assets.lovelace ??
    0n;
  const metadata = positiveBigInt(
    funding.lovelace,
    `${record.walletId} first funding lovelace metadata`,
  );
  if (derived !== metadata) {
    throw new Error(
      `${record.walletId} first funding lovelace metadata ${metadata.toString()} does not match bound output CBOR ${derived.toString()}.`,
    );
  }
  return derived;
};

const normalizedFundingRows = (value: unknown): readonly string[] => {
  const funding = requireObject(value, "fanout wallet latestFunding");
  if (!Array.isArray(funding.fundingUtxos)) {
    throw new Error(
      "fanout wallet latestFunding.fundingUtxos must be an array.",
    );
  }
  return funding.fundingUtxos.map((candidate, index) => {
    const row = requireObject(
      candidate,
      `fanout wallet fundingUtxos[${index.toString()}]`,
    );
    return `${requireString(row.outref, "fanout outref").toLowerCase()}|${requireString(row.outputCbor, "fanout outputCbor").toLowerCase()}|${requireString(row.lovelace, "fanout lovelace")}`;
  });
};

const assertFanoutMatchesWallets = ({
  fanout,
  records,
}: {
  readonly fanout: JsonObject;
  readonly records: readonly StressWalletRecord[];
}): void => {
  if (
    fanout.schemaVersion !== FANOUT_SCHEMA_VERSION ||
    fanout.requestedCount !== records.length ||
    fanout.verifiedWalletCount !== records.length ||
    !Array.isArray(fanout.wallets) ||
    fanout.wallets.length !== records.length
  ) {
    throw new Error(
      "fanout report does not prove the complete retained wallet set.",
    );
  }
  const fanoutByWalletId = new Map<string, JsonObject>();
  for (const [index, candidate] of fanout.wallets.entries()) {
    const entry = requireObject(
      candidate,
      `fanout wallets[${index.toString()}]`,
    );
    const wallet = requireObject(
      entry.wallet,
      `fanout wallets[${index.toString()}].wallet`,
    );
    const walletId = requireString(wallet.walletId, "fanout walletId");
    if (fanoutByWalletId.has(walletId)) {
      throw new Error(`fanout report repeats wallet ${walletId}.`);
    }
    fanoutByWalletId.set(walletId, wallet);
  }
  for (const record of records) {
    const wallet = fanoutByWalletId.get(record.walletId);
    if (
      wallet === undefined ||
      wallet.l2Address !== record.l2Address ||
      wallet.network !== record.network ||
      JSON.stringify(normalizedFundingRows(wallet.latestFunding)) !==
        JSON.stringify(normalizedFundingRows(record.latestFunding))
    ) {
      throw new Error(
        `fanout report wallet/funding evidence does not match ${record.walletId}.`,
      );
    }
  }
};

const exactJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

const retainedTerminalSetSha256 = (
  terminals: readonly VerifiedHistoricalRetainedTerminal[],
): string =>
  createHash("sha256")
    .update(
      terminals
        .map(
          (terminal) =>
            `${terminal.chainId}|${terminal.parentTxHash}#1|${terminal.outputCborSha256}|${terminal.lovelace.toString(10)}`,
        )
        .join("\n"),
    )
    .digest("hex");

export const verifyHistoricalExtensionBaseEvidence = async (
  config: HistoricalExtensionConfig,
  options: {
    readonly authenticatedWalletRecords?: readonly StressWalletRecord[];
  } = {},
): Promise<VerifiedHistoricalExtensionBase> => {
  await Promise.all([
    assertFileIdentity(config.baseCorpus, "base corpus"),
    assertFileIdentity(config.baseIndex, "base index"),
    assertFileIdentity(config.baseManifest, "base manifest"),
    assertFileIdentity(config.baseVerification, "base verification"),
    assertFileIdentity(config.baseBinding, "base Phase 1 binding"),
    assertFileIdentity(config.fanoutReport, "fanout report"),
  ]);
  const [manifest, verification, binding, fanout, baseIndex, records] =
    await Promise.all([
      readJson(config.baseManifest.path, "base corpus manifest"),
      readJson(config.baseVerification.path, "base corpus verification"),
      readJson(config.baseBinding.path, "base Phase 1 binding"),
      readJson(config.fanoutReport.path, "fanout report"),
      parseCorpusIndexEntries(config.baseIndex.path),
      options.authenticatedWalletRecords === undefined
        ? readWalletRecords(config.walletsDir, config.baseChainCount)
        : Promise.resolve(options.authenticatedWalletRecords),
    ]);
  if (records.length !== config.baseChainCount) {
    throw new Error(
      `authenticated wallet record count ${records.length.toString()} does not match ${config.baseChainCount.toString()}.`,
    );
  }
  if (
    manifest.schemaVersion !== BASE_MANIFEST_SCHEMA_VERSION ||
    manifest.chainCount !== config.baseChainCount ||
    manifest.chainDepth !== config.baseDepth
  ) {
    throw new Error(
      "base manifest does not match the configured retained chain shape.",
    );
  }
  const manifestFiles = requireObject(manifest.files, "base manifest files");
  const manifestCorpus = requireObject(
    manifestFiles.corpus,
    "base manifest corpus file",
  );
  const manifestIndex = requireObject(
    manifestFiles.index,
    "base manifest index file",
  );
  const baseRowCount = config.baseChainCount * config.baseDepth;
  if (
    manifestCorpus.sha256 !== config.baseCorpus.sha256 ||
    manifestCorpus.rowCount !== baseRowCount ||
    manifestIndex.sha256 !== config.baseIndex.sha256 ||
    manifestIndex.rowCount !== config.baseChainCount
  ) {
    throw new Error("base manifest file identities do not match their bytes.");
  }
  if (
    verification.schemaVersion !== BASE_VERIFICATION_SCHEMA_VERSION ||
    verification.rowCount !== baseRowCount ||
    verification.chainCount !== config.baseChainCount
  ) {
    throw new Error(
      "base verification does not match the retained corpus shape.",
    );
  }
  const verificationCorpus = requireObject(
    verification.corpus,
    "base verification corpus",
  );
  if (
    verificationCorpus.corpusSha256 !== config.baseCorpus.sha256 ||
    verificationCorpus.indexSha256 !== config.baseIndex.sha256 ||
    verificationCorpus.manifestSha256 !== config.baseManifest.sha256
  ) {
    throw new Error(
      "base verification artifact does not bind the supplied base files.",
    );
  }
  if (binding.schemaVersion !== BASE_BINDING_SCHEMA_VERSION) {
    throw new Error(
      `base binding schema must be ${BASE_BINDING_SCHEMA_VERSION}.`,
    );
  }
  const bindingCorpus = requireObject(binding.corpus, "base binding corpus");
  if (
    bindingCorpus.corpusSha256 !== config.baseCorpus.sha256 ||
    bindingCorpus.indexSha256 !== config.baseIndex.sha256 ||
    bindingCorpus.manifestSha256 !== config.baseManifest.sha256
  ) {
    throw new Error("base Phase 1 binding does not bind the supplied corpus.");
  }
  if (
    baseIndex.length !== config.baseChainCount ||
    baseIndex.some(
      (entry) =>
        entry.rowCount !== config.baseDepth || entry.planShape !== "chain",
    ) ||
    new Set(baseIndex.map((entry) => entry.chainId)).size !==
      config.baseChainCount
  ) {
    throw new Error(
      "base index must contain one exact-depth chain run per unique wallet.",
    );
  }
  const expectedWalletIds = new Set(baseIndex.map((entry) => entry.chainId));
  const walletSetIdentity = computeStressCorpusWalletSetIdentity({
    records,
    expectedWalletCount: config.baseChainCount,
    expectedWalletIds,
  });
  for (const record of records) {
    firstFundingLovelaceFromBoundOutput(record);
  }
  if (
    !exactJson(manifest.walletSetIdentity, walletSetIdentity) ||
    !exactJson(verification.walletSetIdentity, walletSetIdentity) ||
    binding.walletSetSha256 !== walletSetIdentity.walletSetSha256 ||
    binding.fundingSetSha256 !== walletSetIdentity.fundingSetSha256
  ) {
    throw new Error(
      "wallet/funding identity does not match the base manifest, verification, and Phase 1 binding.",
    );
  }
  assertFanoutMatchesWallets({ fanout, records });
  const schedule = createHistoricalExtensionSchedule({
    orderedChainIds: baseIndex.map((entry) => entry.chainId),
    baseDepth: config.baseDepth,
    targetRowCount: config.targetRowCount,
  });
  const sliceIds = new Set(baseIndex.map((entry) => entry.corpusSliceId));
  if (sliceIds.size !== 1) {
    throw new Error("historical extension requires one retained corpus slice.");
  }
  const corpusSliceId = [...sliceIds][0]!;
  const recordsById = new Map(
    records.map((record) => [record.walletId, record]),
  );
  const retainedTerminals = await readVerifiedHistoricalRetainedTerminals({
    corpusPath: config.baseCorpus.path,
    index: baseIndex,
    expectations: schedule.entries.map((entry) => {
      const record = recordsById.get(entry.chainId);
      if (record === undefined) {
        throw new Error(`Missing wallet record for ${entry.chainId}.`);
      }
      return {
        chainId: entry.chainId,
        address: record.l2Address,
        corpusSliceId,
        baseDepth: entry.baseDepth,
      };
    }),
  });
  const network = manifest.network;
  if (network !== "Preprod") {
    throw new Error(
      "historical extension base manifest network must be Preprod.",
    );
  }
  const networkId = nonNegativeBigInt(
    manifest.networkId,
    "base manifest networkId",
  );
  if (networkId !== 0n) {
    throw new Error("historical extension base manifest networkId must be 0.");
  }
  const feeParams = requireObject(
    manifest.feeParams,
    "base manifest feeParams",
  );
  const amountTemplate = requireObject(
    manifest.amountTemplate,
    "base manifest amountTemplate",
  );
  if (amountTemplate.shape !== "self-transfer-change-chain") {
    throw new Error("base manifest amount template is not a chain transfer.");
  }
  return {
    evidence: {
      corpus: config.baseCorpus,
      index: config.baseIndex,
      manifest: config.baseManifest,
      verification: config.baseVerification,
      phase1Binding: {
        ...config.baseBinding,
        schemaVersion: BASE_BINDING_SCHEMA_VERSION,
      },
      fanoutReport: {
        ...config.fanoutReport,
        schemaVersion: FANOUT_SCHEMA_VERSION,
      },
    },
    walletSetIdentity,
    walletRecords: records,
    schedule,
    retainedTerminals,
    retainedTerminalSetSha256: retainedTerminalSetSha256(retainedTerminals),
    build: {
      amountLovelace: positiveBigInt(
        amountTemplate.lovelace,
        "base manifest amount lovelace",
      ),
      minFeeA: nonNegativeBigInt(feeParams.minFeeA, "base manifest minFeeA"),
      minFeeB: nonNegativeBigInt(feeParams.minFeeB, "base manifest minFeeB"),
      network,
      networkId,
      maxSubmitTxCborBytes: positiveSafeInteger(
        manifest.maxSubmitTxCborBytes,
        "base manifest maxSubmitTxCborBytes",
      ),
      corpusSliceId,
      planShape: "chain",
    },
  };
};

export type CorpusWorkerFleetHandle = {
  readonly on: {
    (
      event: "message",
      listener: (message: CorpusWorkerOutput) => void,
    ): unknown;
    (event: "error", listener: (error: Error) => void): unknown;
    (event: "exit", listener: (code: number) => void): unknown;
  };
  readonly terminate: () => Promise<number>;
};

export type CorpusWorkerFleetFactory = (
  input: CorpusWorkerInput,
  index: number,
) => CorpusWorkerFleetHandle;

const createCorpusWorker: CorpusWorkerFleetFactory = (input) =>
  new Worker(resolveWorkerEntry(import.meta.url, "corpus-chain-builder.js"), {
    workerData: { data: input },
  });

export const runCorpusWorkerFleet = async (
  inputs: readonly CorpusWorkerInput[],
  factory: CorpusWorkerFleetFactory = createCorpusWorker,
): Promise<readonly Extract<CorpusWorkerOutput, { readonly type: "done" }>[]> =>
  new Promise((resolveFleet, rejectFleet) => {
    if (inputs.length === 0) {
      rejectFleet(new Error("corpus worker fleet must not be empty."));
      return;
    }
    const workers: CorpusWorkerFleetHandle[] = [];
    const terminalReceived: boolean[] = [];
    const joined: boolean[] = [];
    const results: Array<
      Extract<CorpusWorkerOutput, { readonly type: "done" }> | undefined
    > = [];
    const terminationPromises: Array<Promise<number> | undefined> = [];
    let finished = false;

    const terminateWorker = (index: number): Promise<number> => {
      const existing = terminationPromises[index];
      if (existing !== undefined) {
        return existing;
      }
      const terminating = workers[index]!.terminate();
      terminationPromises[index] = terminating;
      return terminating;
    };

    const terminateAllAndReject = (error: Error): void => {
      if (finished) {
        return;
      }
      finished = true;
      void Promise.allSettled(
        workers.map((_worker, index) => terminateWorker(index)),
      ).then(() => rejectFleet(error));
    };

    const maybeResolve = (): void => {
      if (
        finished ||
        results.length !== inputs.length ||
        results.some((result) => result === undefined) ||
        joined.length !== inputs.length ||
        joined.some((value) => value !== true)
      ) {
        return;
      }
      finished = true;
      resolveFleet(
        results as Extract<CorpusWorkerOutput, { readonly type: "done" }>[],
      );
    };

    for (const [index, input] of inputs.entries()) {
      let worker: CorpusWorkerFleetHandle;
      try {
        worker = factory(input, index);
      } catch (error) {
        terminateAllAndReject(
          error instanceof Error ? error : new Error(String(error)),
        );
        break;
      }
      workers[index] = worker;
      terminalReceived[index] = false;
      joined[index] = false;
      results[index] = undefined;
      worker.on("message", (message) => {
        if (finished || message.type === "progress") {
          return;
        }
        if (terminalReceived[index] === true) {
          terminateAllAndReject(
            new Error(
              `corpus-chain-builder worker ${index.toString()} sent more than one terminal message.`,
            ),
          );
          return;
        }
        terminalReceived[index] = true;
        if (message.type === "failure") {
          terminateAllAndReject(new Error(message.error));
          return;
        }
        results[index] = message;
        void terminateWorker(index).then(
          () => {
            joined[index] = true;
            maybeResolve();
          },
          (error: unknown) => {
            terminateAllAndReject(
              error instanceof Error ? error : new Error(String(error)),
            );
          },
        );
      });
      worker.on("error", (error) => terminateAllAndReject(error));
      worker.on("exit", (code) => {
        if (!finished && terminalReceived[index] !== true) {
          terminateAllAndReject(
            new Error(
              `corpus-chain-builder worker ${index.toString()} exited with ${code.toString()} before a terminal message.`,
            ),
          );
        }
      });
    }
  });

const buildExtensionShards = async ({
  config,
  verifiedBase,
}: {
  readonly config: HistoricalExtensionConfig;
  readonly verifiedBase: VerifiedHistoricalExtensionBase;
}): Promise<
  readonly Extract<CorpusWorkerOutput, { readonly type: "done" }>[]
> => {
  const recordsById = new Map(
    verifiedBase.walletRecords.map((record) => [record.walletId, record]),
  );
  const terminalsById = new Map(
    verifiedBase.retainedTerminals.map((terminal) => [
      terminal.chainId,
      terminal,
    ]),
  );
  const wallets = verifiedBase.schedule.entries.map(
    (entry): CorpusWorkerWallet => {
      const record = recordsById.get(entry.chainId);
      const terminal = terminalsById.get(entry.chainId);
      if (record === undefined || terminal === undefined) {
        throw new Error(
          `Missing wallet record or retained terminal for ${entry.chainId}.`,
        );
      }
      return {
        seedPhrase: record.seedPhrase,
        walletId: record.walletId,
        fundingUtxo: terminal.fundingUtxo,
        corpusSliceId: verifiedBase.build.corpusSliceId,
        depth: entry.extensionRows,
        retainedParentTxHash: terminal.parentTxHash,
      };
    },
  );
  const workerCount = Math.min(config.workers, wallets.length);
  const batchSize = Math.ceil(wallets.length / workerCount);
  const shardDir = join(config.outDir, "extension-shards");
  await mkdir(shardDir, { recursive: true });
  const inputs = Array.from({ length: workerCount }, (_entry, index) => {
    const walletBatch = wallets.slice(
      index * batchSize,
      (index + 1) * batchSize,
    );
    return {
      shardPath: join(
        shardDir,
        `extension.shard-${index.toString().padStart(3, "0")}.ndjson`,
      ),
      walletBatch,
      depth: config.baseDepth,
      amountLovelace: verifiedBase.build.amountLovelace.toString(10),
      feeParams: {
        minFeeA: verifiedBase.build.minFeeA.toString(10),
        minFeeB: verifiedBase.build.minFeeB.toString(10),
      },
      network: verifiedBase.build.network,
      networkId: verifiedBase.build.networkId.toString(10),
      maxSubmitTxCborBytes: verifiedBase.build.maxSubmitTxCborBytes,
      planShape: verifiedBase.build.planShape,
      terminalChangeFloorLovelace:
        verifiedBase.build.amountLovelace.toString(10),
    } satisfies CorpusWorkerInput;
  }).filter((input) => input.walletBatch.length > 0);
  await Promise.all(inputs.map((input) => assertAbsent(input.shardPath)));
  const results =
    inputs.length === 1
      ? [await runCorpusChainWorker(inputs[0]!)]
      : await runCorpusWorkerFleet(inputs);
  for (const [index, result] of results.entries()) {
    if (result.shardPath !== inputs[index]!.shardPath) {
      throw new Error(
        `corpus worker result ${index.toString()} does not preserve its input shard ordinal.`,
      );
    }
  }
  return results;
};

const assertAbsent = async (path: string): Promise<void> => {
  try {
    await stat(path);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT") {
      return;
    }
    throw error;
  }
  throw new Error(
    `Refusing to overwrite historical extension artifact ${path}.`,
  );
};

const publicAssembleResult = (
  assembled: AssembleCorpusResult,
): {
  readonly rowCount: number;
  readonly indexEntryCount: number;
  readonly corpusSha256: string;
  readonly indexSha256: string;
} => ({
  rowCount: assembled.rowCount,
  indexEntryCount: assembled.chainCount,
  corpusSha256: assembled.corpusSha256,
  indexSha256: assembled.indexSha256,
});

const historicalExtensionFundingModel = (
  verifiedBase: VerifiedHistoricalExtensionBase,
): HistoricalExtensionFundingModel => {
  const retainedTerminalLovelaceTotal = verifiedBase.retainedTerminals.reduce(
    (total, terminal) => total + terminal.lovelace,
    0n,
  );
  return {
    source: "cryptographically-verified-retained-terminal-output-1-per-wallet",
    retainedBaseOriginalFundingSetSha256:
      verifiedBase.walletSetIdentity.fundingSetSha256,
    retainedTerminalSetHashAlgorithm:
      HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM,
    retainedTerminalSetSha256: verifiedBase.retainedTerminalSetSha256,
    freshFundingLovelace: "0",
    retainedTerminalLovelaceTotal: retainedTerminalLovelaceTotal.toString(10),
    continuationFundingValueSource:
      "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain",
    amountLovelacePerRow: verifiedBase.build.amountLovelace.toString(10),
    retainedBaseRequestedTransferLovelace: (
      verifiedBase.build.amountLovelace *
      BigInt(verifiedBase.schedule.baseRowCount)
    ).toString(10),
    extensionRequestedTransferLovelace: (
      verifiedBase.build.amountLovelace *
      BigInt(verifiedBase.schedule.extensionRowCount)
    ).toString(10),
    feeFormula: {
      minFeeA: verifiedBase.build.minFeeA.toString(10),
      minFeeB: verifiedBase.build.minFeeB.toString(10),
      formula: "minFeeA * canonicalCborByteLength + minFeeB",
    },
    minimumTerminalChangeLovelacePerChain:
      verifiedBase.build.amountLovelace.toString(10),
    proof:
      "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows",
  };
};

const generateHistoricalCorpusExtensionFromSnapshot = async ({
  config,
  finalOutDir,
  baseEvidence,
  authenticatedWalletRecords,
}: {
  readonly config: HistoricalExtensionConfig;
  readonly finalOutDir: string;
  readonly baseEvidence: HistoricalExtensionBaseEvidence;
  readonly authenticatedWalletRecords: readonly StressWalletRecord[];
}): Promise<HistoricalExtensionGenerationResult> => {
  const verifiedBase = await verifyHistoricalExtensionBaseEvidence(config, {
    authenticatedWalletRecords,
  });
  assertHistoricalExtensionSchedule(verifiedBase.schedule);
  await mkdir(config.outDir, { recursive: true });
  const corpusPath = join(config.outDir, "historical-corpus.ndjson");
  const indexPath = `${corpusPath}.index.ndjson`;
  const manifestPath = `${corpusPath}.manifest.json`;
  const verificationPath = `${corpusPath}.verify.json`;
  const bindingPath = join(config.outDir, "historical-corpus-binding.json");
  const generationResultPath = join(
    config.outDir,
    "historical-corpus-generation-result.json",
  );
  const finalCorpusPath = join(finalOutDir, "historical-corpus.ndjson");
  const finalIndexPath = `${finalCorpusPath}.index.ndjson`;
  const finalManifestPath = `${finalCorpusPath}.manifest.json`;
  const finalVerificationPath = `${finalCorpusPath}.verify.json`;
  const finalBindingPath = join(finalOutDir, "historical-corpus-binding.json");
  const finalGenerationResultPath = join(
    finalOutDir,
    "historical-corpus-generation-result.json",
  );
  await Promise.all(
    [
      corpusPath,
      indexPath,
      manifestPath,
      verificationPath,
      bindingPath,
      generationResultPath,
    ].map(assertAbsent),
  );
  const shardResults = await buildExtensionShards({ config, verifiedBase });
  const assembled = await assembleCorpusShards({
    shardPaths: [
      verifiedBase.evidence.corpus.path,
      ...shardResults.map((result) => result.shardPath),
    ],
    corpusPath,
    indexPath,
  });
  if (
    assembled.rowCount !== verifiedBase.schedule.targetRowCount ||
    assembled.chainCount !== verifiedBase.schedule.baseChainCount * 2
  ) {
    throw new Error(
      "assembled historical extension does not have the exact scheduled row/run count.",
    );
  }
  const generatedAtIso = new Date().toISOString();
  const fundingModel = historicalExtensionFundingModel(verifiedBase);
  const manifest = {
    schemaVersion: HISTORICAL_EXTENSION_MANIFEST_SCHEMA_VERSION,
    claimScope: HISTORICAL_EXTENSION_CLAIM_SCOPE,
    freshLiveClaim: false,
    compatibility: HISTORICAL_EXTENSION_COMPATIBILITY,
    generatedAtIso,
    baseEvidence,
    walletSetIdentity: verifiedBase.walletSetIdentity,
    schedule: verifiedBase.schedule,
    fundingModel,
    files: {
      corpus: {
        path: resolve(finalCorpusPath),
        sha256: assembled.corpusSha256,
        rowCount: assembled.rowCount,
      },
      index: {
        path: resolve(finalIndexPath),
        sha256: assembled.indexSha256,
        rowCount: assembled.chainCount,
      },
      extensionShards: shardResults.map((result) => ({
        path: resolve(
          finalOutDir,
          "extension-shards",
          basename(result.shardPath),
        ),
        sha256: result.sha256,
        rowCount: result.rowCount,
      })),
    },
  };
  await writeFile(manifestPath, `${formatJson(manifest)}\n`, "utf8");
  const verified = await verifyHistoricalExtensionCorpus({
    baseCorpusPath: verifiedBase.evidence.corpus.path,
    baseIndexPath: verifiedBase.evidence.index.path,
    extendedCorpusPath: corpusPath,
    extendedIndexPath: indexPath,
    extendedManifestPath: manifestPath,
    schedule: verifiedBase.schedule,
  });
  const verificationArtifact = {
    schemaVersion: HISTORICAL_EXTENSION_VERIFICATION_SCHEMA_VERSION,
    claimScope: HISTORICAL_EXTENSION_CLAIM_SCOPE,
    freshLiveClaim: false,
    compatibility: HISTORICAL_EXTENSION_COMPATIBILITY,
    verifiedAtIso: new Date().toISOString(),
    baseEvidence,
    walletSetIdentity: verifiedBase.walletSetIdentity,
    schedule: verifiedBase.schedule,
    fundingModel,
    corpus: {
      path: resolve(finalCorpusPath),
      indexPath: resolve(finalIndexPath),
      manifestPath: resolve(finalManifestPath),
      corpusSha256: verified.corpusSha256,
      indexSha256: verified.indexSha256,
      manifestSha256: verified.manifestSha256,
    },
    checks: {
      baseGlobalPrefixByteIdentical: true,
      everyBaseChainPrefixByteIdentical: true,
      everyContinuationMetadataLinkValidByStressCorpusVerifier: true,
      everyRetainedTerminalCanonicalNativeIdentityAndDeclaredIoValid: true,
      everyContinuationCanonicalNativeIdentityAndDeclaredIoValid: true,
      exactTargetRowCount: true,
      ...verified,
    },
  };
  await writeFile(
    verificationPath,
    `${formatJson(verificationArtifact)}\n`,
    "utf8",
  );
  const [manifestSha256, verificationSha256] = await Promise.all([
    sha256File(manifestPath),
    sha256File(verificationPath),
  ]);
  const historicalBinding = {
    schemaVersion: HISTORICAL_EXTENSION_BINDING_SCHEMA_VERSION,
    claimScope: HISTORICAL_EXTENSION_CLAIM_SCOPE,
    freshLiveClaim: false,
    compatibility: HISTORICAL_EXTENSION_COMPATIBILITY,
    generatedAtIso,
    baseEvidence,
    walletSetIdentity: verifiedBase.walletSetIdentity,
    schedule: verifiedBase.schedule,
    fundingModel,
    corpus: {
      path: resolve(finalCorpusPath),
      indexPath: resolve(finalIndexPath),
      manifestPath: resolve(finalManifestPath),
      verificationPath: resolve(finalVerificationPath),
      corpusSha256: verified.corpusSha256,
      indexSha256: verified.indexSha256,
      manifestSha256,
      verificationSha256,
      rowCount: verified.rowCount,
      uniqueChainCount: verified.uniqueChainCount,
      indexEntryCount: verified.indexEntryCount,
    },
  };
  await writeFile(bindingPath, `${formatJson(historicalBinding)}\n`, "utf8");
  const bindingSha256 = await sha256File(bindingPath);
  const generationResult: HistoricalExtensionGenerationResult = {
    schemaVersion: HISTORICAL_EXTENSION_GENERATION_SCHEMA_VERSION,
    claimScope: HISTORICAL_EXTENSION_CLAIM_SCOPE,
    freshLiveClaim: false,
    compatibility: HISTORICAL_EXTENSION_COMPATIBILITY,
    generatedAtIso,
    outDir: resolve(finalOutDir),
    baseEvidence,
    walletSetIdentity: verifiedBase.walletSetIdentity,
    schedule: verifiedBase.schedule,
    fundingModel,
    files: {
      corpus: {
        path: resolve(finalCorpusPath),
        sha256: verified.corpusSha256,
        rowCount: verified.rowCount,
      },
      index: {
        path: resolve(finalIndexPath),
        sha256: verified.indexSha256,
        rowCount: verified.indexEntryCount,
      },
      manifest: {
        path: resolve(finalManifestPath),
        sha256: manifestSha256,
      },
      verification: {
        path: resolve(finalVerificationPath),
        sha256: verificationSha256,
      },
      historicalBinding: {
        path: resolve(finalBindingPath),
        sha256: bindingSha256,
      },
    },
    assembled: publicAssembleResult(assembled),
    verification: verified,
    generationResultPath: resolve(finalGenerationResultPath),
  };
  await writeFile(
    generationResultPath,
    `${formatJson(generationResult)}\n`,
    "utf8",
  );
  return generationResult;
};

const syncDirectory = async (path: string): Promise<void> => {
  const handle = await open(path, "r");
  try {
    await handle.sync();
  } finally {
    await handle.close();
  }
};

const removeStageRoot = async (stageRoot: string): Promise<void> => {
  await chmod(join(stageRoot, "immutable-inputs"), 0o700).catch(() => {});
  await rm(stageRoot, { recursive: true, force: true });
};

export const generateHistoricalCorpusExtension = async (
  config: HistoricalExtensionConfig,
  runtime: HistoricalExtensionRuntime = {},
): Promise<HistoricalExtensionGenerationResult> => {
  if (!config.yes) {
    throw new Error(
      "Refusing to generate historical corpus extension without --yes.",
    );
  }
  const finalOutDir = resolve(config.outDir);
  await mkdir(dirname(finalOutDir), { recursive: true });
  await assertAbsent(finalOutDir);
  const stageRoot = await mkdtemp(
    join(
      dirname(finalOutDir),
      `.historical-extension-${basename(finalOutDir)}-`,
    ),
  );
  await chmod(stageRoot, 0o700);
  let promoted = false;
  try {
    const authenticatedBase = await verifyHistoricalExtensionBaseEvidence({
      ...config,
      outDir: finalOutDir,
    });
    const snapshotConfig = await createImmutableInputSnapshot({
      config: { ...config, outDir: finalOutDir },
      stageRoot,
    });
    await runtime.afterImmutableSnapshot?.();
    const result = await generateHistoricalCorpusExtensionFromSnapshot({
      config: snapshotConfig,
      finalOutDir,
      baseEvidence: publishedBaseEvidence(config),
      authenticatedWalletRecords: authenticatedBase.walletRecords,
    });
    await verifyHistoricalExtensionBaseEvidence({
      ...config,
      outDir: finalOutDir,
    });
    await runtime.beforeAtomicPromotion?.();
    await assertAbsent(finalOutDir);
    await rename(snapshotConfig.outDir, finalOutDir);
    promoted = true;
    await syncDirectory(dirname(finalOutDir));
    await removeStageRoot(stageRoot);
    return result;
  } catch (error) {
    await removeStageRoot(stageRoot).catch(() => {});
    if (promoted) {
      throw new Error(
        `Historical extension was atomically published at ${finalOutDir}, but final directory sync failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
    throw error;
  }
};
