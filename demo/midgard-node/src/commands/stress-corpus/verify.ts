import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { open, readdir, readFile, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import type { Network } from "@lucid-evolution/lucid";

import type { CorpusIndexEntry } from "@/commands/stress-corpus/assemble.js";
import {
  buildCorpusChain,
  type CorpusFeeParams,
  type CorpusFundingUtxo,
} from "@/commands/stress-corpus/build-chain.js";
import {
  computeStressCorpusWalletSetIdentity,
  STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
  STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
  type StressCorpusWalletSetIdentity,
} from "@/commands/stress-corpus/wallet-set-identity.js";
import {
  type OpenLoopCorpusRow,
  parseOpenLoopCorpusLine,
} from "@/commands/stress-open-loop.js";
import {
  parseStressWalletRecord,
  type StressWalletRecord,
} from "@/commands/stress-wallets.js";

export const DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE = 0.001;
export const STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM =
  "sha256-corpus-chain-id-order-v1";
export const STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION =
  "midgard-stress-corpus-verification-v1";
export const STRESS_CORPUS_MANIFEST_SCHEMA_VERSION =
  "midgard-stress-corpus-manifest-v1";

export type StressCorpusManifestV1 = {
  readonly schemaVersion: typeof STRESS_CORPUS_MANIFEST_SCHEMA_VERSION;
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly warmupCount: number;
  readonly cooldownCount: number;
  readonly safetyFactor: number;
  readonly assumedAcceptanceLatencyMs: number;
  readonly chainCount: number;
  readonly chainDepth: number;
  readonly corpusShape: "fanout" | "chain" | "mixed";
  readonly corpusSliceIds: readonly string[];
  readonly generatedAtIso: string;
  readonly generatorGitSha: string;
  readonly lucidMidgardVersion: string;
  readonly feeParams: {
    readonly minFeeA: string;
    readonly minFeeB: string;
  };
  readonly network: Network;
  readonly networkId: string;
  readonly maxSubmitTxCborBytes: number;
  readonly amountTemplate: {
    readonly lovelace: string;
    readonly shape: "self-transfer-change-chain";
  };
  readonly verification: {
    readonly rebuildSampleRate: number;
    readonly rebuildSampleAlgorithm: typeof STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM;
  };
  readonly fundingSummary: {
    readonly walletCount: number;
    readonly perWalletFundingLovelace: string;
    readonly totalFundingLovelace: string;
  };
  readonly walletSetIdentity: StressCorpusWalletSetIdentity;
  readonly sliceSummary: readonly {
    readonly corpusSliceId: string;
    readonly walletCount: number;
    readonly rowCount: number;
  }[];
  readonly files: {
    readonly corpus: {
      readonly path: string;
      readonly sha256: string;
      readonly rowCount: number;
    };
    readonly index: {
      readonly path: string;
      readonly sha256: string;
      readonly rowCount: number;
    };
    readonly shards: readonly string[];
  };
};

export type VerifyStressCorpusRebuildSampleOptions = {
  readonly walletsDir: string;
  readonly amountLovelace: bigint;
  readonly feeParams: CorpusFeeParams;
  readonly network: Network;
  readonly networkId: bigint;
  readonly maxSubmitTxCborBytes: number;
  readonly sampleRate?: number;
  readonly terminalChangeFloorLovelace?: bigint;
};

export type VerifyStressCorpusRebuildSampleResult = {
  readonly algorithm: typeof STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM;
  readonly sampleRate: number;
  readonly checkedChainCount: number;
  readonly checkedRowCount: number;
  readonly sampledChainIds: readonly string[];
  readonly livePreflightEntries: readonly {
    readonly walletId: string;
    readonly l2Address: string;
    readonly firstInputOutref: string;
    readonly outputCborSha256: string;
  }[];
};

export type VerifyStressCorpusOptions = {
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly manifestPath: string;
  readonly rebuildSample?: VerifyStressCorpusRebuildSampleOptions;
  readonly resultOutPath?: string;
};

export type VerifyStressCorpusResult = {
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly manifestPath: string;
  readonly rowCount: number;
  readonly chainCount: number;
  readonly corpusSha256: string;
  readonly indexSha256: string;
  readonly manifestSha256: string;
  readonly walletSetIdentity?: StressCorpusWalletSetIdentity;
  readonly rebuildSample?: VerifyStressCorpusRebuildSampleResult;
  readonly verificationArtifact?: {
    readonly path: string;
    readonly sha256: string;
  };
};

export type StressCorpusVerificationArtifactV1 = {
  readonly schemaVersion: typeof STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION;
  readonly verifiedAtIso: string;
  readonly corpus: {
    readonly path: string;
    readonly indexPath: string;
    readonly manifestPath: string;
    readonly corpusSha256: string;
    readonly indexSha256: string;
    readonly manifestSha256: string;
  };
  readonly rowCount: number;
  readonly chainCount: number;
  readonly walletSetIdentity?: StressCorpusWalletSetIdentity;
  readonly rebuildSample?: VerifyStressCorpusRebuildSampleResult;
};

type ObservedRun = CorpusIndexEntry;

const exactObject = (
  value: unknown,
  label: string,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[] = [],
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object.`);
  }
  const record = value as Record<string, unknown>;
  const missing = requiredKeys.filter((key) => !Object.hasOwn(record, key));
  const allowedKeys = new Set([...requiredKeys, ...optionalKeys]);
  const extra = Object.keys(record).filter((key) => !allowedKeys.has(key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `${label} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}].`,
    );
  }
  return record;
};

const manifestString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string.`);
  }
  return value;
};

const manifestIsoTimestamp = (value: unknown, label: string): string => {
  const timestamp = manifestString(value, label);
  const parsed = Date.parse(timestamp);
  if (Number.isNaN(parsed) || new Date(parsed).toISOString() !== timestamp) {
    throw new Error(`${label} must be a canonical ISO-8601 timestamp.`);
  }
  return timestamp;
};

const manifestInteger = (
  value: unknown,
  label: string,
  minimum: number,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum
  ) {
    throw new Error(
      `${label} must be a safe integer >= ${minimum.toString()}.`,
    );
  }
  return value;
};

const manifestNumber = (
  value: unknown,
  label: string,
  allowZero = false,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isFinite(value) ||
    (allowZero ? value < 0 : value <= 0)
  ) {
    throw new Error(
      `${label} must be a finite ${allowZero ? "non-negative" : "positive"} number.`,
    );
  }
  return value;
};

const manifestDecimal = (value: unknown, label: string): string => {
  const text = manifestString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(text)) {
    throw new Error(`${label} must be a canonical non-negative decimal.`);
  }
  return text;
};

const manifestDigest = (value: unknown, label: string): string => {
  const digest = manifestString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256.`);
  }
  return digest;
};

const artifactIsoTimestamp = (value: unknown, label: string): string => {
  const timestamp = manifestString(value, label);
  const parsed = new Date(timestamp);
  if (Number.isNaN(parsed.valueOf()) || parsed.toISOString() !== timestamp) {
    throw new Error(`${label} must be a canonical ISO timestamp.`);
  }
  return timestamp;
};

export const parseStressCorpusWalletSetIdentityV1 = (
  value: unknown,
  label = "stress corpus walletSetIdentity",
): StressCorpusWalletSetIdentity => {
  const identity = exactObject(value, label, [
    "walletCount",
    "fundingRowCount",
    "uniqueFirstFundingOutrefCount",
    "walletSetHashAlgorithm",
    "walletSetSha256",
    "fundingSetHashAlgorithm",
    "fundingSetSha256",
  ]);
  if (
    identity.walletSetHashAlgorithm !==
      STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM ||
    identity.fundingSetHashAlgorithm !==
      STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM
  ) {
    throw new Error(`${label} hash algorithm is unsupported.`);
  }
  const parsed: StressCorpusWalletSetIdentity = {
    walletCount: manifestInteger(
      identity.walletCount,
      `${label}.walletCount`,
      1,
    ),
    fundingRowCount: manifestInteger(
      identity.fundingRowCount,
      `${label}.fundingRowCount`,
      1,
    ),
    uniqueFirstFundingOutrefCount: manifestInteger(
      identity.uniqueFirstFundingOutrefCount,
      `${label}.uniqueFirstFundingOutrefCount`,
      1,
    ),
    walletSetHashAlgorithm: STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
    walletSetSha256: manifestDigest(
      identity.walletSetSha256,
      `${label}.walletSetSha256`,
    ),
    fundingSetHashAlgorithm: STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
    fundingSetSha256: manifestDigest(
      identity.fundingSetSha256,
      `${label}.fundingSetSha256`,
    ),
  };
  if (
    parsed.fundingRowCount < parsed.walletCount ||
    parsed.uniqueFirstFundingOutrefCount !== parsed.walletCount
  ) {
    throw new Error(`${label} cardinality binding is inconsistent.`);
  }
  return parsed;
};

export const parseStressCorpusRebuildSampleResultV1 = (
  value: unknown,
  label = "stress corpus rebuildSample",
): VerifyStressCorpusRebuildSampleResult => {
  const root = exactObject(value, label, [
    "algorithm",
    "sampleRate",
    "checkedChainCount",
    "checkedRowCount",
    "sampledChainIds",
    "livePreflightEntries",
  ]);
  if (root.algorithm !== STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM) {
    throw new Error(`${label}.algorithm is unsupported.`);
  }
  const sampleRate = manifestNumber(root.sampleRate, `${label}.sampleRate`);
  if (sampleRate > 1) {
    throw new Error(`${label}.sampleRate must be <= 1.`);
  }
  if (!Array.isArray(root.sampledChainIds)) {
    throw new Error(`${label}.sampledChainIds must be an array.`);
  }
  const sampledChainIds = root.sampledChainIds.map((entry, index) =>
    manifestString(entry, `${label}.sampledChainIds[${index.toString()}]`),
  );
  if (new Set(sampledChainIds).size !== sampledChainIds.length) {
    throw new Error(`${label}.sampledChainIds must be unique.`);
  }
  if (!Array.isArray(root.livePreflightEntries)) {
    throw new Error(`${label}.livePreflightEntries must be an array.`);
  }
  const livePreflightEntries = root.livePreflightEntries.map((value, index) => {
    const entryLabel = `${label}.livePreflightEntries[${index.toString()}]`;
    const entry = exactObject(value, entryLabel, [
      "walletId",
      "l2Address",
      "firstInputOutref",
      "outputCborSha256",
    ]);
    const firstInputOutref = manifestString(
      entry.firstInputOutref,
      `${entryLabel}.firstInputOutref`,
    );
    if (!/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(firstInputOutref)) {
      throw new Error(
        `${entryLabel}.firstInputOutref must be a canonical transaction outref.`,
      );
    }
    return {
      walletId: manifestString(entry.walletId, `${entryLabel}.walletId`),
      l2Address: manifestString(entry.l2Address, `${entryLabel}.l2Address`),
      firstInputOutref,
      outputCborSha256: manifestDigest(
        entry.outputCborSha256,
        `${entryLabel}.outputCborSha256`,
      ),
    };
  });
  const checkedChainCount = manifestInteger(
    root.checkedChainCount,
    `${label}.checkedChainCount`,
    1,
  );
  const checkedRowCount = manifestInteger(
    root.checkedRowCount,
    `${label}.checkedRowCount`,
    1,
  );
  if (
    sampledChainIds.length !== checkedChainCount ||
    livePreflightEntries.length !== checkedChainCount ||
    checkedRowCount < checkedChainCount ||
    livePreflightEntries.some(
      (entry, index) => entry.walletId !== sampledChainIds[index],
    )
  ) {
    throw new Error(`${label} cardinality binding is inconsistent.`);
  }
  return {
    algorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    sampleRate,
    checkedChainCount,
    checkedRowCount,
    sampledChainIds,
    livePreflightEntries,
  };
};

export const parseStressCorpusVerificationArtifactV1 = (
  value: unknown,
): StressCorpusVerificationArtifactV1 => {
  const root = exactObject(
    value,
    "stress corpus verification artifact",
    ["schemaVersion", "verifiedAtIso", "corpus", "rowCount", "chainCount"],
    ["walletSetIdentity", "rebuildSample"],
  );
  if (root.schemaVersion !== STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress corpus verification artifact schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const corpus = exactObject(
    root.corpus,
    "stress corpus verification artifact corpus",
    [
      "path",
      "indexPath",
      "manifestPath",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
  );
  const rowCount = manifestInteger(
    root.rowCount,
    "stress corpus verification artifact rowCount",
    1,
  );
  const chainCount = manifestInteger(
    root.chainCount,
    "stress corpus verification artifact chainCount",
    1,
  );
  const walletSetIdentity =
    root.walletSetIdentity === undefined
      ? undefined
      : parseStressCorpusWalletSetIdentityV1(
          root.walletSetIdentity,
          "stress corpus verification artifact walletSetIdentity",
        );
  const rebuildSample =
    root.rebuildSample === undefined
      ? undefined
      : parseStressCorpusRebuildSampleResultV1(
          root.rebuildSample,
          "stress corpus verification artifact rebuildSample",
        );
  if ((walletSetIdentity === undefined) !== (rebuildSample === undefined)) {
    throw new Error(
      "stress corpus verification artifact walletSetIdentity and rebuildSample must be present together.",
    );
  }
  if (
    walletSetIdentity !== undefined &&
    (walletSetIdentity.walletCount !== chainCount ||
      rebuildSample!.checkedChainCount > chainCount ||
      rebuildSample!.checkedRowCount > rowCount)
  ) {
    throw new Error(
      "stress corpus verification artifact cardinality binding is inconsistent.",
    );
  }
  return {
    schemaVersion: STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
    verifiedAtIso: artifactIsoTimestamp(
      root.verifiedAtIso,
      "stress corpus verification artifact verifiedAtIso",
    ),
    corpus: {
      path: manifestString(
        corpus.path,
        "stress corpus verification artifact corpus.path",
      ),
      indexPath: manifestString(
        corpus.indexPath,
        "stress corpus verification artifact corpus.indexPath",
      ),
      manifestPath: manifestString(
        corpus.manifestPath,
        "stress corpus verification artifact corpus.manifestPath",
      ),
      corpusSha256: manifestDigest(
        corpus.corpusSha256,
        "stress corpus verification artifact corpus.corpusSha256",
      ),
      indexSha256: manifestDigest(
        corpus.indexSha256,
        "stress corpus verification artifact corpus.indexSha256",
      ),
      manifestSha256: manifestDigest(
        corpus.manifestSha256,
        "stress corpus verification artifact corpus.manifestSha256",
      ),
    },
    rowCount,
    chainCount,
    ...(walletSetIdentity === undefined ? {} : { walletSetIdentity }),
    ...(rebuildSample === undefined ? {} : { rebuildSample }),
  };
};

export const parseStressCorpusManifest = (
  value: unknown,
): StressCorpusManifestV1 => {
  const root = exactObject(value, "stress corpus manifest", [
    "schemaVersion",
    "targetRateTps",
    "durationMs",
    "warmupCount",
    "cooldownCount",
    "safetyFactor",
    "assumedAcceptanceLatencyMs",
    "chainCount",
    "chainDepth",
    "corpusShape",
    "corpusSliceIds",
    "generatedAtIso",
    "generatorGitSha",
    "lucidMidgardVersion",
    "feeParams",
    "network",
    "networkId",
    "maxSubmitTxCborBytes",
    "amountTemplate",
    "verification",
    "fundingSummary",
    "walletSetIdentity",
    "sliceSummary",
    "files",
  ]);
  if (root.schemaVersion !== STRESS_CORPUS_MANIFEST_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress corpus manifest schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const corpusShape = root.corpusShape;
  if (
    corpusShape !== "fanout" &&
    corpusShape !== "chain" &&
    corpusShape !== "mixed"
  ) {
    throw new Error("stress corpus manifest corpusShape is unsupported.");
  }
  const network = root.network;
  if (network !== "Mainnet" && network !== "Preprod") {
    throw new Error("stress corpus manifest network is unsupported.");
  }
  if (!Array.isArray(root.corpusSliceIds) || root.corpusSliceIds.length === 0) {
    throw new Error(
      "stress corpus manifest corpusSliceIds must be a non-empty array.",
    );
  }
  const corpusSliceIds = root.corpusSliceIds.map((entry, index) =>
    manifestString(entry, `corpusSliceIds[${index.toString()}]`),
  );
  if (new Set(corpusSliceIds).size !== corpusSliceIds.length) {
    throw new Error("stress corpus manifest corpusSliceIds must be unique.");
  }
  const feeParams = exactObject(root.feeParams, "manifest feeParams", [
    "minFeeA",
    "minFeeB",
  ]);
  const amountTemplate = exactObject(
    root.amountTemplate,
    "manifest amountTemplate",
    ["lovelace", "shape"],
  );
  if (amountTemplate.shape !== "self-transfer-change-chain") {
    throw new Error("manifest amountTemplate.shape is unsupported.");
  }
  const verification = exactObject(root.verification, "manifest verification", [
    "rebuildSampleRate",
    "rebuildSampleAlgorithm",
  ]);
  if (
    verification.rebuildSampleAlgorithm !==
    STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM
  ) {
    throw new Error("manifest rebuild sample algorithm is unsupported.");
  }
  const rebuildSampleRate = manifestNumber(
    verification.rebuildSampleRate,
    "manifest verification.rebuildSampleRate",
  );
  if (rebuildSampleRate > 1) {
    throw new Error("manifest verification.rebuildSampleRate must be <= 1.");
  }
  const fundingSummary = exactObject(
    root.fundingSummary,
    "manifest fundingSummary",
    ["walletCount", "perWalletFundingLovelace", "totalFundingLovelace"],
  );
  const walletSetIdentity = exactObject(
    root.walletSetIdentity,
    "manifest walletSetIdentity",
    [
      "walletCount",
      "fundingRowCount",
      "uniqueFirstFundingOutrefCount",
      "walletSetHashAlgorithm",
      "walletSetSha256",
      "fundingSetHashAlgorithm",
      "fundingSetSha256",
    ],
  );
  if (
    walletSetIdentity.walletSetHashAlgorithm !==
      STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM ||
    walletSetIdentity.fundingSetHashAlgorithm !==
      STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM
  ) {
    throw new Error("manifest wallet-set hash algorithm is unsupported.");
  }
  if (!Array.isArray(root.sliceSummary) || root.sliceSummary.length === 0) {
    throw new Error(
      "stress corpus manifest sliceSummary must be a non-empty array.",
    );
  }
  const sliceSummary = root.sliceSummary.map((value, index) => {
    const entry = exactObject(
      value,
      `manifest sliceSummary[${index.toString()}]`,
      ["corpusSliceId", "walletCount", "rowCount"],
    );
    return {
      corpusSliceId: manifestString(
        entry.corpusSliceId,
        `manifest sliceSummary[${index.toString()}].corpusSliceId`,
      ),
      walletCount: manifestInteger(
        entry.walletCount,
        `manifest sliceSummary[${index.toString()}].walletCount`,
        1,
      ),
      rowCount: manifestInteger(
        entry.rowCount,
        `manifest sliceSummary[${index.toString()}].rowCount`,
        1,
      ),
    };
  });
  const files = exactObject(root.files, "manifest files", [
    "corpus",
    "index",
    "shards",
  ]);
  const parseBoundFile = (
    value: unknown,
    label: string,
  ): {
    readonly path: string;
    readonly sha256: string;
    readonly rowCount: number;
  } => {
    const file = exactObject(value, label, ["path", "sha256", "rowCount"]);
    return {
      path: manifestString(file.path, `${label}.path`),
      sha256: manifestDigest(file.sha256, `${label}.sha256`),
      rowCount: manifestInteger(file.rowCount, `${label}.rowCount`, 1),
    };
  };
  if (
    !Array.isArray(files.shards) ||
    files.shards.length === 0 ||
    files.shards.some((path) => typeof path !== "string" || path.length === 0)
  ) {
    throw new Error("manifest files.shards must be a non-empty string array.");
  }
  const parsed: StressCorpusManifestV1 = {
    schemaVersion: STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
    targetRateTps: manifestNumber(root.targetRateTps, "manifest targetRateTps"),
    durationMs: manifestInteger(root.durationMs, "manifest durationMs", 1),
    warmupCount: manifestInteger(root.warmupCount, "manifest warmupCount", 0),
    cooldownCount: manifestInteger(
      root.cooldownCount,
      "manifest cooldownCount",
      0,
    ),
    safetyFactor: manifestNumber(root.safetyFactor, "manifest safetyFactor"),
    assumedAcceptanceLatencyMs: manifestInteger(
      root.assumedAcceptanceLatencyMs,
      "manifest assumedAcceptanceLatencyMs",
      1,
    ),
    chainCount: manifestInteger(root.chainCount, "manifest chainCount", 1),
    chainDepth: manifestInteger(root.chainDepth, "manifest chainDepth", 1),
    corpusShape,
    corpusSliceIds,
    generatedAtIso: manifestIsoTimestamp(
      root.generatedAtIso,
      "manifest generatedAtIso",
    ),
    generatorGitSha: manifestString(
      root.generatorGitSha,
      "manifest generatorGitSha",
    ),
    lucidMidgardVersion: manifestString(
      root.lucidMidgardVersion,
      "manifest lucidMidgardVersion",
    ),
    feeParams: {
      minFeeA: manifestDecimal(feeParams.minFeeA, "manifest feeParams.minFeeA"),
      minFeeB: manifestDecimal(feeParams.minFeeB, "manifest feeParams.minFeeB"),
    },
    network,
    networkId: manifestDecimal(root.networkId, "manifest networkId"),
    maxSubmitTxCborBytes: manifestInteger(
      root.maxSubmitTxCborBytes,
      "manifest maxSubmitTxCborBytes",
      1,
    ),
    amountTemplate: {
      lovelace: manifestDecimal(
        amountTemplate.lovelace,
        "manifest amountTemplate.lovelace",
      ),
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate,
      rebuildSampleAlgorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    },
    fundingSummary: {
      walletCount: manifestInteger(
        fundingSummary.walletCount,
        "manifest fundingSummary.walletCount",
        1,
      ),
      perWalletFundingLovelace: manifestDecimal(
        fundingSummary.perWalletFundingLovelace,
        "manifest fundingSummary.perWalletFundingLovelace",
      ),
      totalFundingLovelace: manifestDecimal(
        fundingSummary.totalFundingLovelace,
        "manifest fundingSummary.totalFundingLovelace",
      ),
    },
    walletSetIdentity: {
      walletCount: manifestInteger(
        walletSetIdentity.walletCount,
        "manifest walletSetIdentity.walletCount",
        1,
      ),
      fundingRowCount: manifestInteger(
        walletSetIdentity.fundingRowCount,
        "manifest walletSetIdentity.fundingRowCount",
        1,
      ),
      uniqueFirstFundingOutrefCount: manifestInteger(
        walletSetIdentity.uniqueFirstFundingOutrefCount,
        "manifest walletSetIdentity.uniqueFirstFundingOutrefCount",
        1,
      ),
      walletSetHashAlgorithm: STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
      walletSetSha256: manifestDigest(
        walletSetIdentity.walletSetSha256,
        "manifest walletSetIdentity.walletSetSha256",
      ),
      fundingSetHashAlgorithm: STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
      fundingSetSha256: manifestDigest(
        walletSetIdentity.fundingSetSha256,
        "manifest walletSetIdentity.fundingSetSha256",
      ),
    },
    sliceSummary,
    files: {
      corpus: parseBoundFile(files.corpus, "manifest files.corpus"),
      index: parseBoundFile(files.index, "manifest files.index"),
      shards: files.shards.map((path, index) =>
        manifestString(path, `manifest files.shards[${index.toString()}]`),
      ),
    },
  };
  const expectedNetworkId = parsed.network === "Mainnet" ? "1" : "0";
  const sliceWalletCount = parsed.sliceSummary.reduce(
    (sum, entry) => sum + entry.walletCount,
    0,
  );
  const sliceIds = parsed.sliceSummary.map((entry) => entry.corpusSliceId);
  const expectedRowCount = parsed.chainCount * parsed.chainDepth;
  const expectedFunding =
    BigInt(parsed.fundingSummary.walletCount) *
    BigInt(parsed.fundingSummary.perWalletFundingLovelace);
  if (
    parsed.networkId !== expectedNetworkId ||
    JSON.stringify(sliceIds) !== JSON.stringify(parsed.corpusSliceIds) ||
    new Set(sliceIds).size !== sliceIds.length ||
    new Set(parsed.files.shards).size !== parsed.files.shards.length ||
    parsed.sliceSummary.some(
      (entry) => entry.rowCount !== entry.walletCount * parsed.chainDepth,
    ) ||
    parsed.files.corpus.rowCount !== expectedRowCount ||
    parsed.files.corpus.rowCount !==
      parsed.sliceSummary.reduce((sum, entry) => sum + entry.rowCount, 0) ||
    parsed.files.index.rowCount !== parsed.chainCount ||
    parsed.chainCount !== parsed.fundingSummary.walletCount ||
    sliceWalletCount !== parsed.fundingSummary.walletCount ||
    parsed.fundingSummary.walletCount !==
      parsed.walletSetIdentity.walletCount ||
    parsed.walletSetIdentity.uniqueFirstFundingOutrefCount !==
      parsed.walletSetIdentity.walletCount ||
    BigInt(parsed.fundingSummary.totalFundingLovelace) !== expectedFunding
  ) {
    throw new Error(
      "stress corpus manifest cardinality binding is inconsistent.",
    );
  }
  return parsed;
};

const sha256File = async (path: string): Promise<string> =>
  new Promise((resolve, reject) => {
    const hash = createHash("sha256");
    const input = createReadStream(path);
    input.on("data", (chunk: string | Buffer) => {
      hash.update(chunk);
    });
    input.on("error", reject);
    input.on("end", () => resolve(hash.digest("hex")));
  });

export const parseStressCorpusIndexLine = (
  line: string,
  index: number,
): CorpusIndexEntry => {
  const parsed = exactObject(
    JSON.parse(line) as unknown,
    `index row ${index.toString()}`,
    [
      "corpusSliceId",
      "planShape",
      "chainId",
      "startByteOffset",
      "endByteOffset",
      "rowCount",
    ],
  );
  if (
    typeof parsed.corpusSliceId !== "string" ||
    (parsed.planShape !== "fanout" &&
      parsed.planShape !== "chain" &&
      parsed.planShape !== "mixed") ||
    typeof parsed.chainId !== "string" ||
    !Number.isSafeInteger(parsed.startByteOffset) ||
    !Number.isSafeInteger(parsed.endByteOffset) ||
    !Number.isSafeInteger(parsed.rowCount) ||
    (parsed.startByteOffset as number) < 0 ||
    (parsed.endByteOffset as number) <= (parsed.startByteOffset as number) ||
    (parsed.rowCount as number) <= 0
  ) {
    throw new Error(
      `index row ${index.toString()} is not a valid corpus index entry.`,
    );
  }
  return {
    corpusSliceId: parsed.corpusSliceId,
    planShape: parsed.planShape,
    chainId: parsed.chainId,
    startByteOffset: parsed.startByteOffset as number,
    endByteOffset: parsed.endByteOffset as number,
    rowCount: parsed.rowCount as number,
  };
};

const parseIndex = async (path: string): Promise<readonly CorpusIndexEntry[]> =>
  (await readFile(path, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => parseStressCorpusIndexLine(line, index + 1));

const closeObservedRun = (
  runs: ObservedRun[],
  currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusRow["planShape"];
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined,
  endByteOffset: number,
): void => {
  if (currentRun === undefined) {
    return;
  }
  runs.push({
    corpusSliceId: currentRun.corpusSliceId,
    planShape: currentRun.planShape,
    chainId: currentRun.chainId,
    startByteOffset: currentRun.startByteOffset,
    endByteOffset,
    rowCount: currentRun.rowCount,
  });
};

const compareIndexEntries = (
  expected: readonly CorpusIndexEntry[],
  observed: readonly CorpusIndexEntry[],
): void => {
  if (expected.length !== observed.length) {
    throw new Error(
      `index entry count ${expected.length.toString()} does not match observed chain runs ${observed.length.toString()}.`,
    );
  }
  for (let i = 0; i < expected.length; i += 1) {
    const lhs = expected[i]!;
    const rhs = observed[i]!;
    if (JSON.stringify(lhs) !== JSON.stringify(rhs)) {
      throw new Error(
        `index entry ${(i + 1).toString()} does not match observed corpus run: expected ${JSON.stringify(lhs)}, observed ${JSON.stringify(rhs)}.`,
      );
    }
  }
};

const walletFilePattern = /^wallet-\d{4}\.json$/u;

const readWalletRecordsById = async (
  walletsDir: string,
): Promise<{
  readonly records: readonly StressWalletRecord[];
  readonly recordsById: ReadonlyMap<string, StressWalletRecord>;
}> => {
  const files = (await readdir(walletsDir))
    .filter((file) => walletFilePattern.test(file))
    .sort();
  const records = await Promise.all(
    files.map(async (file) => {
      return parseStressWalletRecord(
        JSON.parse(await readFile(join(walletsDir, file), "utf8")) as unknown,
      );
    }),
  );
  return {
    records,
    recordsById: new Map(records.map((record) => [record.walletId, record])),
  };
};

const fundingUtxoForRecord = (
  record: StressWalletRecord,
): CorpusFundingUtxo => {
  const funding = record.latestFunding?.fundingUtxos?.[0];
  if (funding === undefined) {
    throw new Error(
      `Stress wallet ${record.walletId} has no latestFunding.fundingUtxos[0]; cannot run corpus rebuild sample.`,
    );
  }
  const [txHash, indexRaw, extra] = funding.outref.split("#");
  if (
    txHash === undefined ||
    indexRaw === undefined ||
    extra !== undefined ||
    !/^[0-9a-f]{64}$/iu.test(txHash) ||
    !/^(0|[1-9][0-9]*)$/u.test(indexRaw)
  ) {
    throw new Error(
      `Stress wallet ${record.walletId} funding outref ${funding.outref} must use <64hex>#<index>.`,
    );
  }
  return {
    txHash: txHash.toLowerCase(),
    outputIndex: Number(indexRaw),
    outputCborHex: funding.outputCbor,
  };
};

const normalizedSampleRate = (sampleRate: number | undefined): number => {
  const parsed = sampleRate ?? DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE;
  if (!Number.isFinite(parsed) || parsed <= 0 || parsed > 1) {
    throw new Error("rebuild sample rate must be > 0 and <= 1.");
  }
  return parsed;
};

const sampleKey = (corpusSha256: string, entry: CorpusIndexEntry): string =>
  createHash("sha256")
    .update(corpusSha256)
    .update("\0")
    .update(entry.chainId)
    .update("\0")
    .update(String(entry.startByteOffset))
    .digest("hex");

const selectRebuildSample = (
  index: readonly CorpusIndexEntry[],
  corpusSha256: string,
  sampleRate: number,
): readonly CorpusIndexEntry[] => {
  if (index.length === 0) {
    return [];
  }
  const sampleCount = Math.max(1, Math.ceil(index.length * sampleRate));
  return [...index]
    .sort((left, right) =>
      sampleKey(corpusSha256, left).localeCompare(
        sampleKey(corpusSha256, right),
      ),
    )
    .slice(0, sampleCount);
};

const readCorpusRangeLines = async (
  corpusPath: string,
  entry: CorpusIndexEntry,
): Promise<readonly string[]> => {
  const byteLength = entry.endByteOffset - entry.startByteOffset;
  if (!Number.isSafeInteger(byteLength) || byteLength <= 0) {
    throw new Error(
      `index entry for ${entry.chainId} has invalid byte range ${entry.startByteOffset.toString()}..${entry.endByteOffset.toString()}.`,
    );
  }
  const file = await open(corpusPath, "r");
  try {
    const buffer = Buffer.alloc(byteLength);
    const { bytesRead } = await file.read(
      buffer,
      0,
      byteLength,
      entry.startByteOffset,
    );
    if (bytesRead !== byteLength) {
      throw new Error(
        `could only read ${bytesRead.toString()} of ${byteLength.toString()} bytes for sampled chain ${entry.chainId}.`,
      );
    }
    const lines = buffer
      .toString("utf8")
      .split("\n")
      .map((line) => line.replace(/\r$/u, ""))
      .filter((line) => line.length > 0);
    if (lines.length !== entry.rowCount) {
      throw new Error(
        `sampled chain ${entry.chainId} index rowCount ${entry.rowCount.toString()} does not match ${lines.length.toString()} corpus rows.`,
      );
    }
    return lines;
  } finally {
    await file.close();
  }
};

const verifyRebuildSample = async ({
  corpusPath,
  index,
  corpusSha256,
  options,
  recordsById,
}: {
  readonly corpusPath: string;
  readonly index: readonly CorpusIndexEntry[];
  readonly corpusSha256: string;
  readonly options: VerifyStressCorpusRebuildSampleOptions;
  readonly recordsById: ReadonlyMap<string, StressWalletRecord>;
}): Promise<VerifyStressCorpusRebuildSampleResult> => {
  const sampleRate = normalizedSampleRate(options.sampleRate);
  const sample = selectRebuildSample(index, corpusSha256, sampleRate);
  let checkedRowCount = 0;
  const livePreflightEntries: Array<
    VerifyStressCorpusRebuildSampleResult["livePreflightEntries"][number]
  > = [];

  for (const entry of sample) {
    const record = recordsById.get(entry.chainId);
    if (record === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no matching stress wallet record in ${options.walletsDir}.`,
      );
    }
    const corpusLines = await readCorpusRangeLines(corpusPath, entry);
    const firstRow = parseOpenLoopCorpusLine(corpusLines[0]!, 1);
    const firstFunding = record.latestFunding?.fundingUtxos?.[0];
    if (firstFunding === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no first wallet funding entry.`,
      );
    }
    if (firstRow.selectedInputOutref !== firstFunding.outref.toLowerCase()) {
      throw new Error(
        `sampled chain ${entry.chainId} first input ${firstRow.selectedInputOutref} does not match wallet funding ${firstFunding.outref}.`,
      );
    }
    livePreflightEntries.push({
      walletId: entry.chainId,
      l2Address: record.l2Address,
      firstInputOutref: firstRow.selectedInputOutref,
      outputCborSha256: createHash("sha256")
        .update(Buffer.from(firstFunding.outputCbor, "hex"))
        .digest("hex"),
    });
    const rebuilt = await buildCorpusChain({
      seedPhrase: record.seedPhrase,
      walletId: record.walletId,
      fundingUtxo: fundingUtxoForRecord(record),
      depth: entry.rowCount,
      amountLovelace: options.amountLovelace,
      feeParams: options.feeParams,
      network: options.network,
      networkId: options.networkId,
      maxSubmitTxCborBytes: options.maxSubmitTxCborBytes,
      corpusSliceId: entry.corpusSliceId,
      planShape: entry.planShape,
      terminalChangeFloorLovelace: options.terminalChangeFloorLovelace,
    });
    if (rebuilt.rows.length !== corpusLines.length) {
      throw new Error(
        `sampled chain ${entry.chainId} rebuilt ${rebuilt.rows.length.toString()} rows, expected ${corpusLines.length.toString()}.`,
      );
    }
    for (let rowOffset = 0; rowOffset < corpusLines.length; rowOffset += 1) {
      const corpusLine = corpusLines[rowOffset]!;
      parseOpenLoopCorpusLine(corpusLine, rowOffset + 1);
      const rebuiltLine = JSON.stringify(rebuilt.rows[rowOffset]!);
      if (corpusLine !== rebuiltLine) {
        throw new Error(
          `rebuild sample mismatch for ${entry.chainId} row ${(rowOffset + 1).toString()}: corpus row is not byte-identical to a fresh build.`,
        );
      }
    }
    checkedRowCount += corpusLines.length;
  }

  return {
    algorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    sampleRate,
    checkedChainCount: sample.length,
    checkedRowCount,
    sampledChainIds: sample.map((entry) => entry.chainId),
    livePreflightEntries,
  };
};

const writeVerificationArtifact = async (
  path: string,
  result: Omit<VerifyStressCorpusResult, "verificationArtifact">,
): Promise<{ readonly path: string; readonly sha256: string }> => {
  const absolutePath = resolve(path);
  const document = parseStressCorpusVerificationArtifactV1({
    schemaVersion: STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
    verifiedAtIso: new Date().toISOString(),
    corpus: {
      path: resolve(result.corpusPath),
      indexPath: resolve(result.indexPath),
      manifestPath: resolve(result.manifestPath),
      corpusSha256: result.corpusSha256,
      indexSha256: result.indexSha256,
      manifestSha256: result.manifestSha256,
    },
    rowCount: result.rowCount,
    chainCount: result.chainCount,
    walletSetIdentity: result.walletSetIdentity,
    rebuildSample: result.rebuildSample,
  });
  await writeFile(
    absolutePath,
    `${JSON.stringify(document, null, 2)}\n`,
    "utf8",
  );
  return { path: absolutePath, sha256: await sha256File(absolutePath) };
};

export const verifyStressCorpus = async (
  options: VerifyStressCorpusOptions,
): Promise<VerifyStressCorpusResult> => {
  const manifest = parseStressCorpusManifest(
    JSON.parse(await readFile(options.manifestPath, "utf8")) as unknown,
  );
  const manifestSha256 = await sha256File(options.manifestPath);
  if (
    resolve(manifest.files.corpus.path) !== resolve(options.corpusPath) ||
    resolve(manifest.files.index.path) !== resolve(options.indexPath)
  ) {
    throw new Error(
      "stress corpus manifest file paths do not bind the requested corpus and index.",
    );
  }
  const expectedIndex = await parseIndex(options.indexPath);
  const seenInputs = new Set<string>();
  const lastByChain = new Map<
    string,
    { readonly txHash: string; readonly changeOutref: string }
  >();
  const observedRuns: ObservedRun[] = [];
  const corpusHash = createHash("sha256");
  let carry = Buffer.alloc(0);
  let byteOffset = 0;
  let rowIndex = 0;
  let currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusRow["planShape"];
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined;

  const processLine = (lineBytes: Buffer, rawLength: number): void => {
    const startByteOffset = byteOffset;
    byteOffset += rawLength;
    const line = lineBytes.toString("utf8").replace(/\r$/u, "");
    if (line.trim().length === 0) {
      return;
    }
    rowIndex += 1;
    const row = parseOpenLoopCorpusLine(line, rowIndex);
    const existingInput = seenInputs.has(row.selectedInputOutref);
    if (existingInput) {
      throw new Error(
        `duplicate selected input ${row.selectedInputOutref} at row ${rowIndex.toString()}.`,
      );
    }
    seenInputs.add(row.selectedInputOutref);
    const previous = lastByChain.get(row.senderWalletId);
    if (previous === undefined) {
      if (row.parentTxHash !== null) {
        throw new Error(
          `row ${rowIndex.toString()} starts chain ${row.senderWalletId} with non-null parentTxHash.`,
        );
      }
    } else {
      if (row.parentTxHash !== previous.txHash) {
        throw new Error(
          `row ${rowIndex.toString()} parentTxHash ${String(row.parentTxHash)} does not match previous chain tx ${previous.txHash}.`,
        );
      }
      if (row.selectedInputOutref !== previous.changeOutref) {
        throw new Error(
          `row ${rowIndex.toString()} selected input ${row.selectedInputOutref} does not spend previous change ${previous.changeOutref}.`,
        );
      }
    }
    if (row.outputOutrefs[1] === undefined) {
      throw new Error(
        `row ${rowIndex.toString()} must include change output outref at index 1.`,
      );
    }
    lastByChain.set(row.senderWalletId, {
      txHash: row.txHash,
      changeOutref: row.outputOutrefs[1],
    });
    if (
      currentRun === undefined ||
      currentRun.chainId !== row.senderWalletId ||
      currentRun.corpusSliceId !== row.corpusSliceId ||
      currentRun.planShape !== row.planShape
    ) {
      closeObservedRun(observedRuns, currentRun, startByteOffset);
      currentRun = {
        corpusSliceId: row.corpusSliceId,
        planShape: row.planShape,
        chainId: row.senderWalletId,
        startByteOffset,
        rowCount: 0,
      };
    }
    currentRun.rowCount += 1;
  };

  for await (const chunk of createReadStream(options.corpusPath)) {
    const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
    corpusHash.update(buffer);
    let pending = Buffer.concat([carry, buffer]);
    let newlineIndex = pending.indexOf(0x0a);
    while (newlineIndex >= 0) {
      processLine(pending.subarray(0, newlineIndex), newlineIndex + 1);
      pending = pending.subarray(newlineIndex + 1);
      newlineIndex = pending.indexOf(0x0a);
    }
    carry = pending;
  }
  if (carry.length > 0) {
    processLine(carry, carry.length);
  }
  closeObservedRun(observedRuns, currentRun, byteOffset);
  compareIndexEntries(expectedIndex, observedRuns);

  const corpusSha256 = corpusHash.digest("hex");
  const indexSha256 = await sha256File(options.indexPath);
  if (manifest.files.corpus.sha256 !== corpusSha256) {
    throw new Error(
      `manifest corpus sha256 ${manifest.files.corpus.sha256} does not match ${corpusSha256}.`,
    );
  }
  if (manifest.files.index.sha256 !== indexSha256) {
    throw new Error(
      `manifest index sha256 ${manifest.files.index.sha256} does not match ${indexSha256}.`,
    );
  }
  if (
    manifest.files.corpus.rowCount !== rowIndex ||
    manifest.files.index.rowCount !== observedRuns.length ||
    manifest.chainCount !== observedRuns.length
  ) {
    throw new Error(
      "stress corpus manifest cardinalities do not match the bound artifacts.",
    );
  }
  const expectedWalletIds = new Set(
    expectedIndex.map((entry) => entry.chainId),
  );
  const walletRecords =
    options.rebuildSample === undefined
      ? undefined
      : await readWalletRecordsById(options.rebuildSample.walletsDir);
  const walletSetIdentity =
    walletRecords === undefined
      ? undefined
      : computeStressCorpusWalletSetIdentity({
          records: walletRecords.records,
          expectedWalletCount: expectedWalletIds.size,
          expectedWalletIds,
        });
  if (
    walletSetIdentity !== undefined &&
    JSON.stringify(manifest.walletSetIdentity) !==
      JSON.stringify(walletSetIdentity)
  ) {
    throw new Error(
      "manifest walletSetIdentity does not match the complete rebuild wallet set.",
    );
  }
  const rebuildSample =
    options.rebuildSample === undefined
      ? undefined
      : await verifyRebuildSample({
          corpusPath: options.corpusPath,
          index: expectedIndex,
          corpusSha256,
          options: options.rebuildSample,
          recordsById: walletRecords!.recordsById,
        });
  const result: Omit<VerifyStressCorpusResult, "verificationArtifact"> = {
    corpusPath: options.corpusPath,
    indexPath: options.indexPath,
    manifestPath: options.manifestPath,
    rowCount: rowIndex,
    chainCount: observedRuns.length,
    corpusSha256,
    indexSha256,
    manifestSha256,
    ...(walletSetIdentity === undefined ? {} : { walletSetIdentity }),
    ...(rebuildSample === undefined ? {} : { rebuildSample }),
  };
  if (options.resultOutPath === undefined) {
    return result;
  }
  return {
    ...result,
    verificationArtifact: await writeVerificationArtifact(
      options.resultOutPath,
      result,
    ),
  };
};
