import { readdir, readFile } from "node:fs/promises";
import { cpus } from "node:os";
import { join } from "node:path";

import type { Network } from "@lucid-evolution/lucid";

import type { CorpusFundingUtxo } from "./stress-corpus/build-chain.js";
import { DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE } from "./stress-corpus/verify.js";
import { type StressCorpusGenerateConfig } from "./stress-corpus-generate.stress-corpus-generate-config.js";
import {
  DEFAULT_STRESS_WALLET_DIR,
  parseStressWalletRecord,
  type StressWalletRecord,
} from "./stress-wallets/index.js";

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

export const parsePositiveRate = (
  value: unknown,
  fieldName: string,
): number => {
  const parsed = parsePositiveNumber(value, fieldName);
  if (parsed > 1) {
    throw new Error(`${fieldName} must be <= 1.`);
  }
  return parsed;
};

export const parsePositiveInteger = (
  value: unknown,
  fieldName: string,
): number => {
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

export const parsePositiveBigInt = (
  value: unknown,
  fieldName: string,
): bigint => {
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

export const parseNonNegativeBigInt = (
  value: unknown,
  fieldName: string,
): bigint => {
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

export const parseNetwork = (
  value: unknown,
  env: NodeJS.ProcessEnv,
): Network => {
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

export const readWalletRecords = async (
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

export const fundingUtxoForRecord = (
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

export const corpusSliceId = ({
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
