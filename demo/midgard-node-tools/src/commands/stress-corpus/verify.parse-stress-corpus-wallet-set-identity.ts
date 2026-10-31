import type { Network } from "@lucid-evolution/lucid";

import type { CorpusIndexEntry } from "./assemble.js";
import { type CorpusFeeParams } from "./build-chain.js";
import {
  STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
  STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
  type StressCorpusWalletSetIdentity,
} from "./wallet-set-identity.js";

export const DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE = 0.001;

export const STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM =
  "sha256-corpus-chain-id-order-v1";

export const STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION =
  "midgard-stress-corpus-verification-v1";

export const STRESS_CORPUS_MANIFEST_SCHEMA_VERSION =
  "midgard-stress-corpus-manifest-v1";

export type StressCorpusManifest = {
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

export type StressCorpusVerificationArtifact = {
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

export type ObservedRun = CorpusIndexEntry;

export const exactObject = (
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

export const manifestString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string.`);
  }
  return value;
};

export const manifestIsoTimestamp = (value: unknown, label: string): string => {
  const timestamp = manifestString(value, label);
  const parsed = Date.parse(timestamp);
  if (Number.isNaN(parsed) || new Date(parsed).toISOString() !== timestamp) {
    throw new Error(`${label} must be a canonical ISO-8601 timestamp.`);
  }
  return timestamp;
};

export const manifestInteger = (
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

export const manifestNumber = (
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

export const manifestDecimal = (value: unknown, label: string): string => {
  const text = manifestString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(text)) {
    throw new Error(`${label} must be a canonical non-negative decimal.`);
  }
  return text;
};

export const manifestDigest = (value: unknown, label: string): string => {
  const digest = manifestString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256.`);
  }
  return digest;
};

export const artifactIsoTimestamp = (value: unknown, label: string): string => {
  const timestamp = manifestString(value, label);
  const parsed = new Date(timestamp);
  if (Number.isNaN(parsed.valueOf()) || parsed.toISOString() !== timestamp) {
    throw new Error(`${label} must be a canonical ISO timestamp.`);
  }
  return timestamp;
};

export const parseStressCorpusWalletSetIdentity = (
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
