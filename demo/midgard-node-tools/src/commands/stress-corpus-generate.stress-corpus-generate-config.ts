import { execFile } from "node:child_process";
import { promisify } from "node:util";

import type { Network } from "@lucid-evolution/lucid";

import { type AssembleCorpusResult } from "./stress-corpus/assemble.js";
import { type StressCorpusPlan } from "./stress-corpus/plan.js";
import { type VerifyStressCorpusRebuildSampleResult } from "./stress-corpus/verify.js";
import { type StressCorpusWalletSetIdentity } from "./stress-corpus/wallet-set-identity.js";

export const execFileAsync = promisify(execFile);

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

export type SerializedStressCorpusPlan = Omit<
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

export const exactGenerationObject = (
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

export const generationString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string.`);
  }
  return value;
};

export const generationInteger = (
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

export const generationNumber = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isFinite(value) || value <= 0) {
    throw new Error(`${label} must be a finite positive number.`);
  }
  return value;
};

export const generationDecimal = (
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

export const generationDigest = (value: unknown, label: string): string => {
  const digest = generationString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256.`);
  }
  return digest;
};
