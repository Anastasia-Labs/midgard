import { execFile } from "node:child_process";
import { mkdir, readdir, readFile, writeFile } from "node:fs/promises";
import { cpus } from "node:os";
import { join, resolve } from "node:path";
import { promisify } from "node:util";
import { Worker } from "node:worker_threads";

import type { Network } from "@lucid-evolution/lucid";
import {
  formatJson,
  networkIdFromName,
} from "midgard-node/commands/command-utils";
import { resolveWorkerEntry } from "midgard-node/fibers/resolve-worker-entry";

import packageJson from "../../package.json" with { type: "json" };
import {
  type CorpusWorkerInput,
  type CorpusWorkerOutput,
  type CorpusWorkerWallet,
  runCorpusChainWorker,
} from "../workers/corpus-chain-builder.js";
import {
  type AssembleCorpusResult,
  assembleCorpusShards,
} from "./stress-corpus/assemble.js";
import type { CorpusFundingUtxo } from "./stress-corpus/build-chain.js";
import {
  planStressCorpus,
  type StressCorpusPlan,
} from "./stress-corpus/plan.js";
import {
  DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE,
  parseStressCorpusIndexLine,
  parseStressCorpusManifest,
  parseStressCorpusRebuildSampleResult,
  parseStressCorpusWalletSetIdentity,
  STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
  STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
  verifyStressCorpus,
  type VerifyStressCorpusOptions,
  type VerifyStressCorpusRebuildSampleResult,
} from "./stress-corpus/verify.js";
import {
  computeStressCorpusWalletSetIdentity,
  type StressCorpusWalletSetIdentity,
} from "./stress-corpus/wallet-set-identity.js";
import {
  DEFAULT_STRESS_WALLET_DIR,
  parseStressWalletRecord,
  type StressWalletRecord,
} from "./stress-wallets.js";

const execFileAsync = promisify(execFile);

export { verifyStressCorpus };

export type StressCorpusFundingSource = "existing" | "fanout";

export type StressCorpusGenerateConfig = {
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly warmupCount: number;
  readonly cooldownCount: number;
  readonly walletCount?: number;
  readonly safetyFactor: number;
  readonly amountLovelace: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly maxSubmitTxCborBytes: number;
  readonly assumedAcceptanceLatencyMs: number;
  readonly walletsDir: string;
  readonly outDir: string;
  readonly workers: number;
  readonly slices: number;
  readonly sliceWalletCounts?: readonly number[];
  readonly corpusSliceIdPrefix: string;
  readonly fundingSource: StressCorpusFundingSource;
  readonly network: Network;
  readonly rebuildSampleRate: number;
  readonly yes: boolean;
};

export type StressCorpusGenerateResult = {
  readonly schemaVersion: "midgard-stress-corpus-generation-v1";
  readonly outDir: string;
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly manifestPath: string;
  readonly plan: StressCorpusPlan;
  readonly walletSetIdentity: StressCorpusWalletSetIdentity;
  readonly assembled: AssembleCorpusResult;
  readonly verified: {
    readonly rowCount: number;
    readonly chainCount: number;
    readonly corpusSha256: string;
    readonly indexSha256: string;
    readonly rebuildSample: VerifyStressCorpusRebuildSampleResult;
    readonly walletSetIdentity: StressCorpusWalletSetIdentity;
    readonly verificationArtifact: {
      readonly path: string;
      readonly sha256: string;
    };
  };
};

type SerializedStressCorpusPlan = Omit<
  StressCorpusPlan,
  | "amountLovelace"
  | "estimatedFeePerTxLovelace"
  | "perWalletFundingLovelace"
  | "totalFundingLovelace"
  | "estimatedCorpusBytes"
> & {
  readonly amountLovelace: string;
  readonly estimatedFeePerTxLovelace: string;
  readonly perWalletFundingLovelace: string;
  readonly totalFundingLovelace: string;
  readonly estimatedCorpusBytes: string;
};

export type StressCorpusGenerationArtifact = Omit<
  StressCorpusGenerateResult,
  "plan"
> & {
  readonly plan: SerializedStressCorpusPlan;
};

const exactGenerationObject = (
  value: unknown,
  label: string,
  keys: readonly string[],
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object.`);
  }
  const record = value as Record<string, unknown>;
  const missing = keys.filter((key) => !Object.hasOwn(record, key));
  const extra = Object.keys(record).filter((key) => !keys.includes(key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `${label} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}].`,
    );
  }
  return record;
};

const generationString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string.`);
  }
  return value;
};

const generationInteger = (
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

const generationNumber = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isFinite(value) || value <= 0) {
    throw new Error(`${label} must be a finite positive number.`);
  }
  return value;
};

const generationDecimal = (
  value: unknown,
  label: string,
  allowZero: boolean,
): string => {
  const decimal = generationString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(decimal)) {
    throw new Error(`${label} must be a canonical non-negative decimal.`);
  }
  if (!allowZero && decimal === "0") {
    throw new Error(`${label} must be greater than zero.`);
  }
  return decimal;
};

const generationDigest = (value: unknown, label: string): string => {
  const digest = generationString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256.`);
  }
  return digest;
};

export const parseStressCorpusGenerationArtifact = (
  value: unknown,
): StressCorpusGenerationArtifact => {
  const root = exactGenerationObject(
    value,
    "stress corpus generation artifact",
    [
      "schemaVersion",
      "outDir",
      "corpusPath",
      "indexPath",
      "manifestPath",
      "plan",
      "walletSetIdentity",
      "assembled",
      "verified",
    ],
  );
  if (root.schemaVersion !== "midgard-stress-corpus-generation-v1") {
    throw new Error(
      `Unsupported stress corpus generation artifact schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const planDocument = exactGenerationObject(
    root.plan,
    "stress corpus generation artifact plan",
    [
      "targetRateTps",
      "durationMs",
      "warmupCount",
      "cooldownCount",
      "rowCount",
      "walletCount",
      "chainDepth",
      "amountLovelace",
      "estimatedFeePerTxLovelace",
      "perWalletFundingLovelace",
      "totalFundingLovelace",
      "estimatedCorpusBytes",
      "assumedAcceptanceLatencyMs",
      "safetyFactor",
      "corpusShape",
      "interleavingPlan",
    ],
  );
  if (
    planDocument.corpusShape !== "fanout" &&
    planDocument.corpusShape !== "chain" &&
    planDocument.corpusShape !== "mixed"
  ) {
    throw new Error(
      "stress corpus generation artifact plan.corpusShape is unsupported.",
    );
  }
  if (planDocument.interleavingPlan !== "grouped-by-chain") {
    throw new Error(
      "stress corpus generation artifact plan.interleavingPlan is unsupported.",
    );
  }
  const plan: SerializedStressCorpusPlan = {
    targetRateTps: generationNumber(
      planDocument.targetRateTps,
      "stress corpus generation artifact plan.targetRateTps",
    ),
    durationMs: generationInteger(
      planDocument.durationMs,
      "stress corpus generation artifact plan.durationMs",
      1,
    ),
    warmupCount: generationInteger(
      planDocument.warmupCount,
      "stress corpus generation artifact plan.warmupCount",
      0,
    ),
    cooldownCount: generationInteger(
      planDocument.cooldownCount,
      "stress corpus generation artifact plan.cooldownCount",
      0,
    ),
    rowCount: generationInteger(
      planDocument.rowCount,
      "stress corpus generation artifact plan.rowCount",
      1,
    ),
    walletCount: generationInteger(
      planDocument.walletCount,
      "stress corpus generation artifact plan.walletCount",
      1,
    ),
    chainDepth: generationInteger(
      planDocument.chainDepth,
      "stress corpus generation artifact plan.chainDepth",
      1,
    ),
    amountLovelace: generationDecimal(
      planDocument.amountLovelace,
      "stress corpus generation artifact plan.amountLovelace",
      false,
    ),
    estimatedFeePerTxLovelace: generationDecimal(
      planDocument.estimatedFeePerTxLovelace,
      "stress corpus generation artifact plan.estimatedFeePerTxLovelace",
      true,
    ),
    perWalletFundingLovelace: generationDecimal(
      planDocument.perWalletFundingLovelace,
      "stress corpus generation artifact plan.perWalletFundingLovelace",
      false,
    ),
    totalFundingLovelace: generationDecimal(
      planDocument.totalFundingLovelace,
      "stress corpus generation artifact plan.totalFundingLovelace",
      false,
    ),
    estimatedCorpusBytes: generationDecimal(
      planDocument.estimatedCorpusBytes,
      "stress corpus generation artifact plan.estimatedCorpusBytes",
      false,
    ),
    assumedAcceptanceLatencyMs: generationInteger(
      planDocument.assumedAcceptanceLatencyMs,
      "stress corpus generation artifact plan.assumedAcceptanceLatencyMs",
      1,
    ),
    safetyFactor: generationNumber(
      planDocument.safetyFactor,
      "stress corpus generation artifact plan.safetyFactor",
    ),
    corpusShape: planDocument.corpusShape,
    interleavingPlan: "grouped-by-chain",
  };
  if (
    plan.rowCount !== plan.walletCount * plan.chainDepth ||
    BigInt(plan.totalFundingLovelace) !==
      BigInt(plan.perWalletFundingLovelace) * BigInt(plan.walletCount)
  ) {
    throw new Error(
      "stress corpus generation artifact plan cardinality binding is inconsistent.",
    );
  }

  const assembledDocument = exactGenerationObject(
    root.assembled,
    "stress corpus generation artifact assembled",
    [
      "corpusPath",
      "indexPath",
      "rowCount",
      "chainCount",
      "corpusSha256",
      "indexSha256",
      "indexEntries",
    ],
  );
  if (
    !Array.isArray(assembledDocument.indexEntries) ||
    assembledDocument.indexEntries.length === 0
  ) {
    throw new Error(
      "stress corpus generation artifact assembled.indexEntries must be a non-empty array.",
    );
  }
  const indexEntries = assembledDocument.indexEntries.map((entry, index) =>
    parseStressCorpusIndexLine(JSON.stringify(entry), index + 1),
  );
  const assembled: AssembleCorpusResult = {
    corpusPath: generationString(
      assembledDocument.corpusPath,
      "stress corpus generation artifact assembled.corpusPath",
    ),
    indexPath: generationString(
      assembledDocument.indexPath,
      "stress corpus generation artifact assembled.indexPath",
    ),
    rowCount: generationInteger(
      assembledDocument.rowCount,
      "stress corpus generation artifact assembled.rowCount",
      1,
    ),
    chainCount: generationInteger(
      assembledDocument.chainCount,
      "stress corpus generation artifact assembled.chainCount",
      1,
    ),
    corpusSha256: generationDigest(
      assembledDocument.corpusSha256,
      "stress corpus generation artifact assembled.corpusSha256",
    ),
    indexSha256: generationDigest(
      assembledDocument.indexSha256,
      "stress corpus generation artifact assembled.indexSha256",
    ),
    indexEntries,
  };
  if (
    assembled.chainCount !== indexEntries.length ||
    assembled.rowCount !==
      indexEntries.reduce((sum, entry) => sum + entry.rowCount, 0) ||
    indexEntries[0]!.startByteOffset !== 0 ||
    indexEntries.some(
      (entry, index) =>
        index > 0 &&
        entry.startByteOffset !== indexEntries[index - 1]!.endByteOffset,
    )
  ) {
    throw new Error(
      "stress corpus generation artifact assembled cardinality binding is inconsistent.",
    );
  }

  const verifiedDocument = exactGenerationObject(
    root.verified,
    "stress corpus generation artifact verified",
    [
      "rowCount",
      "chainCount",
      "corpusSha256",
      "indexSha256",
      "rebuildSample",
      "walletSetIdentity",
      "verificationArtifact",
    ],
  );
  const verificationArtifact = exactGenerationObject(
    verifiedDocument.verificationArtifact,
    "stress corpus generation artifact verified.verificationArtifact",
    ["path", "sha256"],
  );
  const walletSetIdentity = parseStressCorpusWalletSetIdentity(
    root.walletSetIdentity,
    "stress corpus generation artifact walletSetIdentity",
  );
  const verifiedWalletSetIdentity = parseStressCorpusWalletSetIdentity(
    verifiedDocument.walletSetIdentity,
    "stress corpus generation artifact verified.walletSetIdentity",
  );
  const rebuildSample = parseStressCorpusRebuildSampleResult(
    verifiedDocument.rebuildSample,
    "stress corpus generation artifact verified.rebuildSample",
  );
  const verified = {
    rowCount: generationInteger(
      verifiedDocument.rowCount,
      "stress corpus generation artifact verified.rowCount",
      1,
    ),
    chainCount: generationInteger(
      verifiedDocument.chainCount,
      "stress corpus generation artifact verified.chainCount",
      1,
    ),
    corpusSha256: generationDigest(
      verifiedDocument.corpusSha256,
      "stress corpus generation artifact verified.corpusSha256",
    ),
    indexSha256: generationDigest(
      verifiedDocument.indexSha256,
      "stress corpus generation artifact verified.indexSha256",
    ),
    rebuildSample,
    walletSetIdentity: verifiedWalletSetIdentity,
    verificationArtifact: {
      path: generationString(
        verificationArtifact.path,
        "stress corpus generation artifact verified.verificationArtifact.path",
      ),
      sha256: generationDigest(
        verificationArtifact.sha256,
        "stress corpus generation artifact verified.verificationArtifact.sha256",
      ),
    },
  };
  const outDir = generationString(
    root.outDir,
    "stress corpus generation artifact outDir",
  );
  const corpusPath = generationString(
    root.corpusPath,
    "stress corpus generation artifact corpusPath",
  );
  const indexPath = generationString(
    root.indexPath,
    "stress corpus generation artifact indexPath",
  );
  const manifestPath = generationString(
    root.manifestPath,
    "stress corpus generation artifact manifestPath",
  );
  if (
    resolve(corpusPath) !== resolve(join(outDir, "corpus.ndjson")) ||
    resolve(indexPath) !== resolve(`${corpusPath}.index.ndjson`) ||
    resolve(manifestPath) !== resolve(`${corpusPath}.manifest.json`) ||
    resolve(verified.verificationArtifact.path) !==
      resolve(`${corpusPath}.verify.json`) ||
    corpusPath !== assembled.corpusPath ||
    indexPath !== assembled.indexPath ||
    plan.rowCount !== assembled.rowCount ||
    plan.rowCount !== verified.rowCount ||
    plan.walletCount !== assembled.chainCount ||
    plan.walletCount !== verified.chainCount ||
    plan.walletCount !== walletSetIdentity.walletCount ||
    assembled.corpusSha256 !== verified.corpusSha256 ||
    assembled.indexSha256 !== verified.indexSha256 ||
    JSON.stringify(walletSetIdentity) !==
      JSON.stringify(verifiedWalletSetIdentity) ||
    rebuildSample.checkedChainCount > verified.chainCount ||
    rebuildSample.checkedRowCount > verified.rowCount
  ) {
    throw new Error(
      "stress corpus generation artifact result binding is inconsistent.",
    );
  }
  return {
    schemaVersion: "midgard-stress-corpus-generation-v1",
    outDir,
    corpusPath,
    indexPath,
    manifestPath,
    plan,
    walletSetIdentity,
    assembled,
    verified,
  };
};

const parsePositiveNumber = (value: unknown, fieldName: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string"
        ? Number(value)
        : NaN;
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`${fieldName} must be positive.`);
  }
  return parsed;
};

const parsePositiveRate = (value: unknown, fieldName: string): number => {
  const parsed = parsePositiveNumber(value, fieldName);
  if (parsed > 1) {
    throw new Error(`${fieldName} must be <= 1.`);
  }
  return parsed;
};

const parsePositiveInteger = (value: unknown, fieldName: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string"
        ? Number(value)
        : NaN;
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${fieldName} must be a positive safe integer.`);
  }
  return parsed;
};

const parseNonNegativeInteger = (value: unknown, fieldName: string): number => {
  const parsed =
    typeof value === "number"
      ? value
      : typeof value === "string"
        ? Number(value)
        : NaN;
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${fieldName} must be a non-negative safe integer.`);
  }
  return parsed;
};

const parseSliceWalletCounts = (
  value: unknown,
): readonly number[] | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error("--slice-wallet-counts must be a comma-separated list.");
  }
  const counts = value
    .split(",")
    .map((entry, index) =>
      parsePositiveInteger(
        entry.trim(),
        `--slice-wallet-counts[${index.toString()}]`,
      ),
    );
  if (counts.length < 2) {
    throw new Error("--slice-wallet-counts must define at least two slices.");
  }
  return counts;
};

const parsePositiveBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value !== "string" && typeof value !== "number") {
    throw new Error(`${fieldName} must be a positive integer.`);
  }
  const text = String(value);
  if (!/^\d+$/u.test(text)) {
    throw new Error(`${fieldName} must be a positive integer.`);
  }
  const parsed = BigInt(text);
  if (parsed <= 0n) {
    throw new Error(`${fieldName} must be greater than zero.`);
  }
  return parsed;
};

const parseNonNegativeBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value !== "string" && typeof value !== "number") {
    throw new Error(`${fieldName} must be a non-negative integer.`);
  }
  const text = String(value);
  if (!/^\d+$/u.test(text)) {
    throw new Error(`${fieldName} must be a non-negative integer.`);
  }
  return BigInt(text);
};

const defaultOutDir = (): string =>
  join(".stress-corpus", new Date().toISOString().replace(/[:.]/gu, "-"));

const parseString = (
  value: unknown,
  fallback: string,
  fieldName: string,
): string => {
  const resolved = value ?? fallback;
  if (typeof resolved !== "string") {
    throw new Error(`${fieldName} must be a string.`);
  }
  return resolved;
};

const parseNetwork = (value: unknown, env: NodeJS.ProcessEnv): Network => {
  const raw = parseString(value, env.NETWORK ?? "Preprod", "--network");
  return raw === "Mainnet" ? "Mainnet" : "Preprod";
};

export const parseStressCorpusGenerateConfig = (
  input: Record<string, unknown>,
  env: NodeJS.ProcessEnv = process.env,
): StressCorpusGenerateConfig => {
  const minFeeA = input.minFeeA ?? env.MIN_FEE_A;
  const minFeeB = input.minFeeB ?? env.MIN_FEE_B;
  const maxSubmitTxCborBytes =
    input.maxSubmitTxCborBytes ?? env.MAX_SUBMIT_TX_CBOR_BYTES;
  if (minFeeA === undefined || minFeeB === undefined) {
    throw new Error(
      "stress-corpus-generate requires --min-fee-a/--min-fee-b or MIN_FEE_A/MIN_FEE_B.",
    );
  }
  if (maxSubmitTxCborBytes === undefined) {
    throw new Error(
      "stress-corpus-generate requires --max-submit-tx-cbor-bytes or MAX_SUBMIT_TX_CBOR_BYTES.",
    );
  }
  const fundingSource = parseString(
    input.fundingSource,
    "existing",
    "--funding-source",
  );
  if (fundingSource !== "existing" && fundingSource !== "fanout") {
    throw new Error("--funding-source must be existing or fanout.");
  }
  const sliceWalletCounts = parseSliceWalletCounts(input.sliceWalletCounts);
  return {
    targetRateTps: parsePositiveNumber(
      input.targetRateTps,
      "--target-rate-tps",
    ),
    durationMs: parsePositiveInteger(input.durationMs, "--duration-ms"),
    warmupCount: parseNonNegativeInteger(
      input.warmupCount ?? 0,
      "--warmup-count",
    ),
    cooldownCount: parseNonNegativeInteger(
      input.cooldownCount ?? 0,
      "--cooldown-count",
    ),
    ...(input.walletCount === undefined
      ? {}
      : {
          walletCount: parsePositiveInteger(
            input.walletCount,
            "--wallet-count",
          ),
        }),
    safetyFactor: parsePositiveNumber(
      input.safetyFactor ?? "1.1",
      "--safety-factor",
    ),
    amountLovelace: parsePositiveBigInt(
      input.amountLovelace ?? "1000000",
      "--amount-lovelace",
    ),
    minFeeA: parseNonNegativeBigInt(minFeeA, "--min-fee-a"),
    minFeeB: parseNonNegativeBigInt(minFeeB, "--min-fee-b"),
    maxSubmitTxCborBytes: parsePositiveInteger(
      maxSubmitTxCborBytes,
      "--max-submit-tx-cbor-bytes",
    ),
    assumedAcceptanceLatencyMs: parsePositiveInteger(
      input.assumedAcceptanceLatencyMs ?? "1000",
      "--assumed-acceptance-latency-ms",
    ),
    walletsDir: parseString(
      input.walletsDir,
      DEFAULT_STRESS_WALLET_DIR,
      "--wallets-dir",
    ),
    outDir: parseString(input.outDir, defaultOutDir(), "--out-dir"),
    workers: parsePositiveInteger(
      input.workers ?? String(Math.max(1, cpus().length - 1)),
      "--workers",
    ),
    slices:
      sliceWalletCounts?.length ??
      parsePositiveInteger(input.slices ?? "1", "--slices"),
    ...(sliceWalletCounts === undefined ? {} : { sliceWalletCounts }),
    corpusSliceIdPrefix: parseString(
      input.corpusSliceIdPrefix,
      "default",
      "--corpus-slice-id-prefix",
    ),
    fundingSource,
    network: parseNetwork(input.network, env),
    rebuildSampleRate: parsePositiveRate(
      input.rebuildSampleRate ??
        DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE.toString(),
      "--rebuild-sample-rate",
    ),
    yes: input.yes === true,
  };
};

const walletFilePattern = /^wallet-\d{4}\.json$/u;

const readWalletRecords = async (
  walletsDir: string,
  count: number,
): Promise<readonly StressWalletRecord[]> => {
  const files = (await readdir(walletsDir))
    .filter((file) => walletFilePattern.test(file))
    .sort();
  if (files.length !== count) {
    throw new Error(
      `wallets-dir ${walletsDir} has ${files.length.toString()} wallet records, expected exactly ${count.toString()} for the current run.`,
    );
  }
  const records = await Promise.all(
    files.map(async (file) =>
      parseStressWalletRecord(
        JSON.parse(await readFile(join(walletsDir, file), "utf8")) as unknown,
      ),
    ),
  );
  return [...records].sort((left, right) => left.index - right.index);
};

const fundingUtxoForRecord = (
  record: StressWalletRecord,
): CorpusFundingUtxo => {
  const funding = record.latestFunding?.fundingUtxos?.[0];
  if (funding === undefined) {
    throw new Error(
      `Stress wallet ${record.walletId} has no latestFunding.fundingUtxos[0]; run stress-wallets:prepare/fanout with this version before offline corpus generation.`,
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

const corpusSliceId = ({
  walletIndex,
  slices,
  prefix,
  sliceWalletCounts,
}: {
  readonly walletIndex: number;
  readonly slices: number;
  readonly prefix: string;
  readonly sliceWalletCounts?: readonly number[];
}): string => {
  if (slices === 1) {
    return prefix;
  }
  if (sliceWalletCounts === undefined) {
    return `${prefix}-${((walletIndex % slices) + 1).toString()}`;
  }
  let exclusiveUpperBound = 0;
  for (const [index, count] of sliceWalletCounts.entries()) {
    exclusiveUpperBound += count;
    if (walletIndex < exclusiveUpperBound) {
      return `${prefix}-${(index + 1).toString()}`;
    }
  }
  throw new Error(
    `wallet index ${walletIndex.toString()} is outside --slice-wallet-counts`,
  );
};

const gitSha = async (): Promise<string> => {
  try {
    const result = await execFileAsync("git", ["rev-parse", "HEAD"], {
      cwd: process.cwd(),
    });
    return result.stdout.trim();
  } catch {
    return "unknown";
  }
};

const workerInputForBatch = ({
  shardPath,
  walletBatch,
  plan,
  config,
}: {
  readonly shardPath: string;
  readonly walletBatch: readonly CorpusWorkerWallet[];
  readonly plan: StressCorpusPlan;
  readonly config: StressCorpusGenerateConfig;
}): CorpusWorkerInput => ({
  shardPath,
  walletBatch,
  depth: plan.chainDepth,
  amountLovelace: config.amountLovelace.toString(10),
  feeParams: {
    minFeeA: config.minFeeA.toString(10),
    minFeeB: config.minFeeB.toString(10),
  },
  network: config.network,
  networkId: networkIdFromName(config.network).toString(10),
  maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
  planShape: "chain",
  terminalChangeFloorLovelace: config.amountLovelace.toString(10),
});

const runWorkerProcess = async (
  input: CorpusWorkerInput,
): Promise<Extract<CorpusWorkerOutput, { readonly type: "done" }>> =>
  new Promise((resolve, reject) => {
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "corpus-chain-builder.js"),
      {
        workerData: { data: input },
      },
    );
    let settled = false;
    worker.on("message", (message: CorpusWorkerOutput) => {
      if (message.type === "progress") {
        return;
      }
      settled = true;
      worker.terminate().catch(() => undefined);
      if (message.type === "failure") {
        reject(new Error(message.error));
      } else {
        resolve(message);
      }
    });
    worker.on("error", (error) => {
      settled = true;
      reject(error);
    });
    worker.on("exit", (code) => {
      if (!settled && code !== 0) {
        reject(
          new Error(
            `corpus-chain-builder worker exited with ${code.toString()}`,
          ),
        );
      }
    });
  });

const runShardBuilders = async ({
  wallets,
  config,
  plan,
}: {
  readonly wallets: readonly CorpusWorkerWallet[];
  readonly config: StressCorpusGenerateConfig;
  readonly plan: StressCorpusPlan;
}): Promise<readonly string[]> => {
  const workerCount = Math.min(config.workers, wallets.length);
  const batchSize = Math.ceil(wallets.length / workerCount);
  const shardDir = join(config.outDir, "shards");
  await mkdir(shardDir, { recursive: true });
  const inputs = Array.from({ length: workerCount }, (_entry, workerIndex) => {
    const start = workerIndex * batchSize;
    const walletBatch = wallets.slice(start, start + batchSize);
    return workerInputForBatch({
      shardPath: join(
        shardDir,
        `corpus.shard-${workerIndex.toString().padStart(2, "0")}.ndjson`,
      ),
      walletBatch,
      plan,
      config,
    });
  }).filter((input) => input.walletBatch.length > 0);
  const results =
    config.workers === 1
      ? [await runCorpusChainWorker(inputs[0]!)]
      : await Promise.all(inputs.map(runWorkerProcess));
  return results
    .sort((left, right) => left.shardPath.localeCompare(right.shardPath))
    .map((result) => result.shardPath);
};

export const generateStressCorpus = async (
  config: StressCorpusGenerateConfig,
): Promise<StressCorpusGenerateResult> => {
  if (!config.yes) {
    throw new Error("Refusing to generate stress corpus without --yes.");
  }
  if (config.fundingSource === "fanout") {
    throw new Error(
      "stress-corpus-generate --funding-source fanout is reserved for stress-wallets:fanout; generate from existing verified wallet funding snapshots.",
    );
  }
  const plan = planStressCorpus({
    targetRateTps: config.targetRateTps,
    durationMs: config.durationMs,
    warmupCount: config.warmupCount,
    cooldownCount: config.cooldownCount,
    walletCount: config.walletCount,
    safetyFactor: config.safetyFactor,
    amountLovelace: config.amountLovelace,
    minFeeA: config.minFeeA,
    minFeeB: config.minFeeB,
    assumedAcceptanceLatencyMs: config.assumedAcceptanceLatencyMs,
  });
  if (config.sliceWalletCounts !== undefined) {
    const configuredWalletTotal = config.sliceWalletCounts.reduce(
      (sum, count) => sum + count,
      0,
    );
    if (configuredWalletTotal !== plan.walletCount) {
      throw new Error(
        `--slice-wallet-counts sum ${configuredWalletTotal.toString()} must equal planned walletCount ${plan.walletCount.toString()}.`,
      );
    }
  }
  const records = await readWalletRecords(config.walletsDir, plan.walletCount);
  const walletSetIdentity = computeStressCorpusWalletSetIdentity({
    records,
    expectedWalletCount: plan.walletCount,
  });
  const wallets = records.map(
    (record, index): CorpusWorkerWallet => ({
      seedPhrase: record.seedPhrase,
      walletId: record.walletId,
      fundingUtxo: fundingUtxoForRecord(record),
      corpusSliceId: corpusSliceId({
        walletIndex: index,
        slices: config.slices,
        prefix: config.corpusSliceIdPrefix,
        ...(config.sliceWalletCounts === undefined
          ? {}
          : { sliceWalletCounts: config.sliceWalletCounts }),
      }),
    }),
  );
  await mkdir(config.outDir, { recursive: true });
  const shardPaths = await runShardBuilders({ wallets, config, plan });
  const corpusPath = join(config.outDir, "corpus.ndjson");
  const indexPath = `${corpusPath}.index.ndjson`;
  const manifestPath = `${corpusPath}.manifest.json`;
  const assembled = await assembleCorpusShards({
    shardPaths,
    corpusPath,
    indexPath,
  });
  const manifest = {
    schemaVersion: STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
    targetRateTps: config.targetRateTps,
    durationMs: config.durationMs,
    warmupCount: config.warmupCount,
    cooldownCount: config.cooldownCount,
    safetyFactor: config.safetyFactor,
    assumedAcceptanceLatencyMs: config.assumedAcceptanceLatencyMs,
    chainCount: assembled.chainCount,
    chainDepth: plan.chainDepth,
    corpusShape: plan.corpusShape,
    corpusSliceIds: [...new Set(wallets.map((wallet) => wallet.corpusSliceId))],
    generatedAtIso: new Date().toISOString(),
    generatorGitSha: await gitSha(),
    lucidMidgardVersion:
      packageJson.dependencies["@al-ft/lucid-midgard"] ?? "unknown",
    feeParams: {
      minFeeA: config.minFeeA.toString(10),
      minFeeB: config.minFeeB.toString(10),
    },
    network: config.network,
    networkId: networkIdFromName(config.network).toString(10),
    maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
    amountTemplate: {
      lovelace: config.amountLovelace.toString(10),
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate: config.rebuildSampleRate,
      rebuildSampleAlgorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    },
    fundingSummary: {
      walletCount: plan.walletCount,
      perWalletFundingLovelace: plan.perWalletFundingLovelace.toString(10),
      totalFundingLovelace: plan.totalFundingLovelace.toString(10),
    },
    walletSetIdentity,
    sliceSummary: [
      ...new Set(wallets.map((wallet) => wallet.corpusSliceId)),
    ].map((sliceId) => {
      const walletCount = wallets.filter(
        (wallet) => wallet.corpusSliceId === sliceId,
      ).length;
      return {
        corpusSliceId: sliceId,
        walletCount,
        rowCount: walletCount * plan.chainDepth,
      };
    }),
    files: {
      corpus: {
        path: corpusPath,
        sha256: assembled.corpusSha256,
        rowCount: assembled.rowCount,
      },
      index: {
        path: indexPath,
        sha256: assembled.indexSha256,
        rowCount: assembled.chainCount,
      },
      shards: shardPaths,
    },
  };
  const canonicalManifest = parseStressCorpusManifest(
    JSON.parse(formatJson(manifest)) as unknown,
  );
  await writeFile(manifestPath, `${formatJson(canonicalManifest)}\n`, "utf8");
  const verified = await verifyStressCorpus({
    corpusPath,
    indexPath,
    manifestPath,
    rebuildSample: {
      walletsDir: config.walletsDir,
      amountLovelace: config.amountLovelace,
      feeParams: {
        minFeeA: config.minFeeA,
        minFeeB: config.minFeeB,
      },
      network: config.network,
      networkId: networkIdFromName(config.network),
      maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
      sampleRate: config.rebuildSampleRate,
      terminalChangeFloorLovelace: config.amountLovelace,
    },
    resultOutPath: `${corpusPath}.verify.json`,
  });
  const result: StressCorpusGenerateResult = {
    schemaVersion: "midgard-stress-corpus-generation-v1",
    outDir: config.outDir,
    corpusPath,
    indexPath,
    manifestPath,
    plan,
    walletSetIdentity,
    assembled,
    verified: {
      rowCount: verified.rowCount,
      chainCount: verified.chainCount,
      corpusSha256: verified.corpusSha256,
      indexSha256: verified.indexSha256,
      rebuildSample: verified.rebuildSample!,
      walletSetIdentity: verified.walletSetIdentity!,
      verificationArtifact: verified.verificationArtifact!,
    },
  };
  parseStressCorpusGenerationArtifact(
    JSON.parse(formatJson(result)) as unknown,
  );
  return result;
};

export const parseStressCorpusVerifyConfig = (
  input: Record<string, unknown>,
  env: NodeJS.ProcessEnv = process.env,
): VerifyStressCorpusOptions => {
  if (typeof input.corpusPath !== "string" || input.corpusPath.length === 0) {
    throw new Error("--corpus-path is required.");
  }
  const indexPath =
    typeof input.indexPath === "string" && input.indexPath.length > 0
      ? input.indexPath
      : `${input.corpusPath}.index.ndjson`;
  const rebuildWalletsDir =
    typeof input.rebuildWalletsDir === "string" &&
    input.rebuildWalletsDir.length > 0
      ? input.rebuildWalletsDir
      : undefined;
  const minFeeA = input.minFeeA ?? env.MIN_FEE_A;
  const minFeeB = input.minFeeB ?? env.MIN_FEE_B;
  const maxSubmitTxCborBytes =
    input.maxSubmitTxCborBytes ?? env.MAX_SUBMIT_TX_CBOR_BYTES;
  const network = parseNetwork(input.network, env);
  return {
    corpusPath: input.corpusPath,
    indexPath,
    manifestPath:
      typeof input.manifestPath === "string" && input.manifestPath.length > 0
        ? input.manifestPath
        : `${input.corpusPath}.manifest.json`,
    ...(rebuildWalletsDir === undefined
      ? {}
      : {
          rebuildSample: {
            walletsDir: rebuildWalletsDir,
            amountLovelace: parsePositiveBigInt(
              input.amountLovelace ?? "1000000",
              "--amount-lovelace",
            ),
            feeParams: {
              minFeeA: parseNonNegativeBigInt(minFeeA, "--min-fee-a"),
              minFeeB: parseNonNegativeBigInt(minFeeB, "--min-fee-b"),
            },
            network,
            networkId: networkIdFromName(network),
            maxSubmitTxCborBytes: parsePositiveInteger(
              maxSubmitTxCborBytes,
              "--max-submit-tx-cbor-bytes",
            ),
            sampleRate: parsePositiveRate(
              input.rebuildSampleRate ??
                DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE.toString(),
              "--rebuild-sample-rate",
            ),
            terminalChangeFloorLovelace: parsePositiveBigInt(
              input.amountLovelace ?? "1000000",
              "--amount-lovelace",
            ),
          },
        }),
    ...(typeof input.resultOut === "string" && input.resultOut.length > 0
      ? { resultOutPath: input.resultOut }
      : {}),
  };
};
