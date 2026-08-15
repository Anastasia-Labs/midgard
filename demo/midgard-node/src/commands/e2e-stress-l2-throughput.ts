import { spawn } from "node:child_process";
import { createReadStream, createWriteStream } from "node:fs";
import { appendFile, mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import readline from "node:readline";
import { isDeepStrictEqual } from "node:util";

import { type Network, walletFromSeed } from "@lucid-evolution/lucid";

import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
  parseAddressArgument,
  parseNodeEndpoint,
  type ResolvedWalletSeedPhrase,
  resolveWalletSeedPhrase,
} from "@/commands/command-utils.js";
import type { GroundTruthMetrics } from "@/commands/stress-db-metrics.js";
import type { EnvironmentFingerprint } from "@/commands/stress-environment-fingerprint.js";
import {
  buildOpenLoopPlacementProof,
  type NoOpCalibrationSummary,
  type OpenLoopCorpusPlan,
  type OpenLoopCorpusRow,
  type OpenLoopCorpusShape,
  type OpenLoopPlacementProof,
  type OpenLoopSubmitRecord,
  type OpenLoopSubmitSummary,
  type OpenLoopWorkloadProfile,
  parseOpenLoopCorpusLine,
  summarizeOpenLoopSubmissions,
} from "@/commands/stress-open-loop.js";
import {
  buildStressMetrics,
  flattenStressMetricRows,
  roundMetric,
  type StressFullFinalityDrainProof,
  type StressMetrics,
  type StressStageMetricDbSources,
} from "@/commands/stress-stage-metrics.js";
import {
  parseSubmitL2TransferConfig,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "@/commands/submit-l2-transfer.js";
import {
  arrayOf,
  booleanValue,
  exactLiteral,
  exactRecord,
  finiteNumber,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  nonNegativeNumber,
  nullableNonEmptyString,
  nullableNonNegativeNumber,
  oneOf,
  positiveInteger,
  stringArray,
} from "@/e2e/exact-artifact.js";
import { percentileOfUnsorted as percentile } from "@/percentile.js";
import { sleep } from "@/sleep.js";

export const E2E_L2_STRESS_CONFIG_SCHEMA_VERSION =
  "midgard-e2e-l2-stress-config-v1";
export const E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION =
  "midgard-e2e-l2-stress-summary-v1";

export type E2EL2StressMode = "serial-chain" | "parallel-fanout";
export type E2EL2StressLoadModel =
  | "closed-loop-smoke"
  | "open-loop-upper-bound";
export type E2EL2StressClassification =
  | "closed_loop_smoke"
  | "full_pipeline_sustained"
  | "ingress_ok_commit_failed"
  | "admission_bottleneck"
  | "validation_bottleneck"
  | "commit_planner_bottleneck"
  | "da_bottleneck"
  | "merge_bottleneck"
  | "provider_bottleneck"
  | "observer_overloaded"
  | "client_overloaded"
  | "corpus_exhausted"
  | "duplicate_submission";
export type E2EL2StressMeasurementPolicy = {
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly syntheticVsProduction:
    | "synthetic_admission_diagnostic"
    | "production_end_user_path";
  readonly advanceOn: "accepted" | "scheduled_submit";
  readonly primaryStageMetric:
    | "metrics.l2Admission.perSecond"
    | "metrics.durableAdmission.perSecond";
  readonly finalityObservation: "post-submit-bounded" | "aggregate-window";
  readonly submissionWindowExcludesCommitDrain: true;
  readonly fullFinalityRequiresDrainProof: true;
};
export type E2EL2StressRateSemantics =
  | "burst_cycle_rate"
  | "offered_tps_uncalibrated";

export type E2EL2StressSubmissionState = {
  readonly status: "submitted" | "failed";
  readonly submittedAt: string | null;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressAcceptanceState = {
  readonly status:
    | "accepted"
    | "rejected"
    | "timeout"
    | "not_observed"
    | "not_submitted";
  readonly acceptedAt?: string;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressFinalityState = {
  readonly status: "committed" | "rejected" | "timeout" | "not_observed";
  readonly committedAt?: string;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressTransaction = {
  readonly index: number;
  readonly phase: "stress";
  readonly txHash: string | null;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly string[];
  readonly submission: E2EL2StressSubmissionState;
  readonly acceptance: E2EL2StressAcceptanceState;
  readonly finality: E2EL2StressFinalityState;
  readonly workerIndex: number;
  readonly walletSeedSource: string;
};

export type E2EL2StressFinalityObserverSummary = {
  readonly mode: "post-submit-bounded" | "aggregate-window";
  readonly maxConcurrentRequests: number;
  readonly maxObservedConcurrentRequests: number;
  readonly observedTransactionCount: number;
  readonly pollRequestCount: number;
  readonly batchCount: number;
  readonly errorCount: number;
};

export type E2EL2StressSummary = {
  readonly schemaVersion: typeof E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION;
  readonly runId: string;
  readonly status: "completed" | "interrupted";
  readonly interruptedReason?: string;
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly corpusShape?: OpenLoopCorpusShape;
  readonly classification: E2EL2StressClassification;
  readonly rateSemantics: E2EL2StressRateSemantics;
  readonly burstCycleRatePerSecond: number | null;
  readonly mode: E2EL2StressMode;
  readonly measurementPolicy: E2EL2StressMeasurementPolicy;
  readonly openLoop?: {
    readonly targetRateTps: number;
    readonly durationMs: number;
    readonly maxInFlight: number;
    readonly corpus: OpenLoopCorpusPlan;
    readonly submission: OpenLoopSubmitSummary;
    readonly calibration?: NoOpCalibrationSummary;
    readonly placement: OpenLoopPlacementProof;
  };
  readonly requestedCount: number;
  readonly notStartedCount: number;
  readonly submittedCount: number;
  readonly submissionFailedCount: number;
  readonly acceptedCount: number;
  readonly acceptanceNotObservedCount: number;
  readonly acceptanceTimedOutCount: number;
  readonly finalityTimedOutCount: number;
  readonly observedCommittedCount: number;
  readonly unknownFinalityCount: number;
  readonly rejectedCount: number;
  readonly concurrency: number;
  readonly finalityObserver: E2EL2StressFinalityObserverSummary;
  readonly startedAt: string;
  readonly submissionFinishedAt: string;
  readonly finishedAt: string;
  readonly submissionDurationMs: number;
  readonly durationMs: number;
  readonly metrics: StressMetrics;
  readonly groundTruth?: GroundTruthMetrics;
  readonly fingerprint?: EnvironmentFingerprint;
  readonly latencyMs: {
    readonly submitP50: number;
    readonly submitP95: number;
    readonly acceptanceP50: number;
    readonly acceptanceP95: number;
    readonly commitP50: number;
    readonly commitP95: number;
  };
  readonly artifactPaths: {
    readonly configJson: string;
    readonly eventsNdjson: string;
    readonly summaryJson: string;
    readonly summaryMarkdown: string;
    readonly engineReportJson?: string;
    readonly engineEventsNdjson?: string;
    readonly submitRecordsNdjson?: string;
    readonly noOpCalibrationJson?: string;
  };
  readonly transactions: readonly E2EL2StressTransaction[];
};

export type E2EL2StressRunResult = {
  readonly summary: E2EL2StressSummary;
  readonly configJsonPath: string;
  readonly eventsNdjsonPath: string;
  readonly summaryJsonPath: string;
  readonly summaryMarkdownPath: string;
};

export type StressSubmitTransferRequest = {
  readonly index: number;
  readonly phase: "stress";
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly walletAddress: string;
  readonly destinationAddress: string;
  readonly walletSeedSource: string;
};

export type StressSubmitTransfer = (
  request: StressSubmitTransferRequest,
) => Promise<SubmitL2TransferResult>;

type StressWallet = {
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly address: string;
};

export type E2EL2StressConfig = {
  readonly runId: string;
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly mode: E2EL2StressMode;
  readonly corpusShape: OpenLoopCorpusShape;
  readonly count: number;
  readonly concurrency: number;
  readonly lovelace: bigint;
  readonly feeHeadroomLovelace: bigint;
  readonly nodeEndpoint: string;
  readonly corpusPath?: string;
  readonly corpusSliceId: string;
  readonly targetRateTps: number;
  readonly openLoopDurationMs: number;
  readonly openLoopWarmupCount: number;
  readonly openLoopCooldownCount: number;
  readonly openLoopMaxInFlight: number;
  readonly noOpCalibrationEndpoint?: string;
  readonly requireNoOpCalibration: boolean;
  readonly noOpCalibrationDurationMs: number;
  readonly aggregateObserverIntervalMs: number;
  readonly destinationAddress?: string;
  readonly pollIntervalMs: number | undefined;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
  readonly submitRequestTimeoutMs: number;
  readonly acceptanceTimeoutMs: number;
  readonly commitObservationTimeoutMs: number;
  readonly finalityObserverMaxConcurrentRequests: number;
  readonly maxSubmissionFailures: number;
  readonly outDir: string;
  readonly network: Network;
  readonly allowUnsafeBounds: boolean;
  readonly primaryWallet?: StressWallet;
  readonly stressWallets: readonly StressWallet[];
};

export type ParseE2EL2StressOptions = {
  readonly endpoint?: string;
  readonly loadModel?: string;
  readonly workloadProfile?: string;
  readonly mode?: string;
  readonly corpusShape?: string;
  readonly corpusPath?: string;
  readonly corpusSliceId?: string;
  readonly targetRateTps?: string;
  readonly openLoopDurationMs?: string;
  readonly openLoopWarmupCount?: string;
  readonly openLoopCooldownCount?: string;
  readonly openLoopMaxInFlight?: string;
  readonly noOpCalibrationEndpoint?: string;
  readonly requireNoOpCalibration?: boolean;
  readonly noOpCalibrationDurationMs?: string;
  readonly aggregateObserverIntervalMs?: string;
  readonly count?: string;
  readonly concurrency?: string;
  readonly lovelace?: string;
  readonly feeHeadroomLovelace?: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly stressWalletSeedPhraseEnvs?: readonly string[];
  readonly l2Address?: string;
  readonly runId?: string;
  readonly outDir?: string;
  readonly pollIntervalMs?: string;
  readonly pollInitialIntervalMs?: string;
  readonly pollMaxIntervalMs?: string;
  readonly submitRequestTimeoutMs?: string;
  readonly acceptanceTimeoutMs?: string;
  readonly commitObservationTimeoutMs?: string;
  readonly finalityObserverMaxConcurrentRequests?: string;
  readonly maxSubmissionFailures?: string;
  readonly network?: Network;
  readonly env?: NodeJS.ProcessEnv;
  readonly allowUnsafeBounds?: boolean;
};

export type E2EL2StressRuntime = {
  readonly submitTransfer: StressSubmitTransfer;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly fetch?: typeof fetch;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly abortSignal?: AbortSignal;
  readonly collectStageMetricSources?: (input: {
    readonly txHashes: readonly string[];
  }) => Promise<StressStageMetricDbSources>;
  readonly collectGroundTruthMetrics?: (input: {
    readonly windowStart: string;
    readonly windowEnd: string;
    readonly txHashSample: readonly string[];
    readonly offeredCount: number;
    readonly calibrationProofRef?: string | null;
  }) => Promise<GroundTruthMetrics>;
  readonly collectEnvironmentFingerprint?: (input: {
    readonly calibrationProofRef?: string | null;
  }) => Promise<EnvironmentFingerprint>;
  readonly collectAggregateObserverSample?: (input: {
    readonly at: string;
    readonly runId: string;
    readonly loadModel: E2EL2StressLoadModel;
  }) => Promise<unknown>;
  readonly fullFinalityDrainProof?: StressFullFinalityDrainProof;
  readonly runCanonicalEngine?: (input: {
    readonly config: E2EL2StressConfig;
    readonly paths: CanonicalEngineArtifactPaths;
    readonly signal?: AbortSignal;
  }) => Promise<CanonicalEngineRunResult>;
};

export type CanonicalEngineArtifactPaths = {
  readonly engineReportJson: string;
  readonly engineEventsNdjson: string;
  readonly submitRecordsNdjson: string;
  readonly noopCalibrationJson: string;
  readonly stdoutLog: string;
  readonly stderrLog: string;
};

export type CanonicalEngineRunResult = {
  readonly exitCode: number;
  readonly signal: NodeJS.Signals | null;
};

const DEFAULT_COUNT = 25;
const DEFAULT_CONCURRENCY = 1;
const DEFAULT_LOVELACE = 1_000_000n;
const DEFAULT_FEE_HEADROOM_LOVELACE = 500_000n;
const DEFAULT_POLL_INTERVAL_MS = 2_000;
const DEFAULT_POLL_INITIAL_INTERVAL_MS = 75;
const DEFAULT_POLL_MAX_INTERVAL_MS = 1_000;
const DEFAULT_POLL_BACKOFF_MULTIPLIER = 2;
const DEFAULT_SUBMIT_REQUEST_TIMEOUT_MS = 300_000;
const DEFAULT_ACCEPTANCE_TIMEOUT_MS = 600_000;
const DEFAULT_COMMIT_OBSERVATION_TIMEOUT_MS = 600_000;
const DEFAULT_FINALITY_OBSERVER_MAX_CONCURRENT_REQUESTS = 4;
const DEFAULT_MAX_SUBMISSION_FAILURES = 0;
const MAX_DEFAULT_COUNT = 500;
const MAX_DEFAULT_CONCURRENCY = 16;
const DEFAULT_OPEN_LOOP_TARGET_RATE_TPS = 100;
const DEFAULT_OPEN_LOOP_DURATION_MS = 10_000;
const DEFAULT_OPEN_LOOP_MAX_IN_FLIGHT = 256;
const DEFAULT_NO_OP_CALIBRATION_DURATION_MS = 5_000;
const DEFAULT_AGGREGATE_OBSERVER_INTERVAL_MS = 1_000;

export const E2E_L2_STRESS_MEASUREMENT_POLICY: E2EL2StressMeasurementPolicy = {
  loadModel: "closed-loop-smoke",
  workloadProfile: "production-end-user",
  syntheticVsProduction: "production_end_user_path",
  advanceOn: "accepted",
  primaryStageMetric: "metrics.l2Admission.perSecond",
  finalityObservation: "post-submit-bounded",
  submissionWindowExcludesCommitDrain: true,
  fullFinalityRequiresDrainProof: true,
};

const timestampForPath = (date = new Date()): string =>
  date
    .toISOString()
    .replaceAll(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

class StressInterruptedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "StressInterruptedError";
  }
}

const abortReason = (signal: AbortSignal | undefined): string => {
  const reason = signal?.reason as unknown;
  if (reason instanceof Error) {
    return reason.message;
  }
  if (typeof reason === "string" && reason.length > 0) {
    return reason;
  }
  return "stress run interrupted";
};

const isAbortLikeError = (error: unknown): boolean =>
  error instanceof StressInterruptedError ||
  (error instanceof Error &&
    (error.name === "AbortError" || error.name === "TimeoutError"));

const throwIfAborted = (signal: AbortSignal | undefined): void => {
  if (signal?.aborted === true) {
    throw new StressInterruptedError(abortReason(signal));
  }
};

const signalWasAborted = (signal: AbortSignal | undefined): boolean =>
  signal?.aborted === true;

const sleepWithAbort = async (
  sleepImpl: (ms: number) => Promise<void>,
  ms: number,
  signal: AbortSignal | undefined,
): Promise<void> => {
  if (ms <= 0) {
    throwIfAborted(signal);
    return;
  }
  if (signal === undefined) {
    await sleepImpl(ms);
    return;
  }
  throwIfAborted(signal);
  let onAbort: (() => void) | undefined;
  try {
    await Promise.race([
      sleepImpl(ms),
      new Promise<never>((_resolve, reject) => {
        onAbort = (): void => {
          reject(new StressInterruptedError(abortReason(signal)));
        };
        signal.addEventListener("abort", onAbort, { once: true });
      }),
    ]);
  } finally {
    if (onAbort !== undefined) {
      signal.removeEventListener("abort", onAbort);
    }
  }
  throwIfAborted(signal);
};

const nextPollDelayMs = (
  attempt: number,
  config: Pick<
    E2EL2StressConfig,
    "pollIntervalMs" | "pollInitialIntervalMs" | "pollMaxIntervalMs"
  >,
): number => {
  if (config.pollIntervalMs !== undefined) {
    return config.pollIntervalMs;
  }
  const scaled =
    config.pollInitialIntervalMs * DEFAULT_POLL_BACKOFF_MULTIPLIER ** attempt;
  return Math.min(config.pollMaxIntervalMs, scaled);
};

const parsePositiveInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer.`);
  }
  return parsed;
};

const parseNonNegativeInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer.`);
  }
  return parsed;
};

const parsePositiveNumber = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  const parsed = Number(raw);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive number.`);
  }
  return parsed;
};

const parsePositiveBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = BigInt(raw);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

const parseNonNegativeBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(raw);
};

const parseMode = (value: string | undefined): E2EL2StressMode => {
  const normalized = value?.trim() || "serial-chain";
  if (normalized === "serial-chain" || normalized === "parallel-fanout") {
    return normalized;
  }
  throw new Error(
    `--mode must be "serial-chain" or "parallel-fanout", got "${value}".`,
  );
};

const parseLoadModel = (value: string | undefined): E2EL2StressLoadModel => {
  const normalized = value?.trim() || "closed-loop-smoke";
  if (
    normalized === "closed-loop-smoke" ||
    normalized === "open-loop-upper-bound"
  ) {
    return normalized;
  }
  throw new Error(
    `--load-model must be "closed-loop-smoke" or "open-loop-upper-bound", got "${value}".`,
  );
};

const parseWorkloadProfile = ({
  value,
  loadModel,
}: {
  readonly value: string | undefined;
  readonly loadModel: E2EL2StressLoadModel;
}): OpenLoopWorkloadProfile => {
  const normalized =
    value?.trim() ||
    (loadModel === "open-loop-upper-bound"
      ? "synthetic-admission"
      : "production-end-user");
  if (
    normalized === "synthetic-admission" ||
    normalized === "production-end-user"
  ) {
    return normalized;
  }
  throw new Error(
    `--workload-profile must be "synthetic-admission" or "production-end-user", got "${value}".`,
  );
};

const parseCorpusShape = (value: string | undefined): OpenLoopCorpusShape => {
  const normalized = value?.trim() || "fanout";
  if (
    normalized === "fanout" ||
    normalized === "chain" ||
    normalized === "mixed"
  ) {
    return normalized;
  }
  throw new Error(
    `--corpus-shape must be "fanout", "chain", or "mixed", got "${value}".`,
  );
};

const measurementPolicyForConfig = (
  config: Pick<E2EL2StressConfig, "loadModel" | "workloadProfile">,
): E2EL2StressMeasurementPolicy => ({
  loadModel: config.loadModel,
  workloadProfile: config.workloadProfile,
  syntheticVsProduction:
    config.workloadProfile === "synthetic-admission"
      ? "synthetic_admission_diagnostic"
      : "production_end_user_path",
  advanceOn:
    config.loadModel === "open-loop-upper-bound"
      ? "scheduled_submit"
      : "accepted",
  primaryStageMetric:
    config.loadModel === "open-loop-upper-bound"
      ? "metrics.durableAdmission.perSecond"
      : "metrics.l2Admission.perSecond",
  finalityObservation:
    config.loadModel === "open-loop-upper-bound"
      ? "aggregate-window"
      : "post-submit-bounded",
  submissionWindowExcludesCommitDrain: true,
  fullFinalityRequiresDrainProof: true,
});

const deriveWalletAddress = (
  resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase,
  network: Network,
): string =>
  walletFromSeed(resolvedWalletSeedPhrase.seedPhrase, { network }).address;

const resolveStressWallet = ({
  walletSeedPhrase,
  walletSeedPhraseEnv,
  env,
  network,
}: {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv: string;
  readonly env: NodeJS.ProcessEnv;
  readonly network: Network;
}): StressWallet => {
  const resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
    walletSeedPhrase,
    walletSeedPhraseEnv,
    env,
  });
  return {
    resolvedWalletSeedPhrase,
    address: deriveWalletAddress(resolvedWalletSeedPhrase, network),
  };
};

type StressWalletResolutionResult =
  | {
      readonly ok: true;
      readonly wallet: StressWallet;
    }
  | {
      readonly ok: false;
      readonly envName: string;
      readonly error: string;
    };

const resolveStressWallets = ({
  envNames,
  env,
  network,
}: {
  readonly envNames: readonly string[];
  readonly env: NodeJS.ProcessEnv;
  readonly network: Network;
}): readonly StressWallet[] => {
  const results: readonly StressWalletResolutionResult[] = envNames.map(
    (envName) => {
      try {
        return {
          ok: true,
          wallet: resolveStressWallet({
            walletSeedPhraseEnv: envName,
            env,
            network,
          }),
        };
      } catch (error) {
        return { ok: false, envName, error: errorMessage(error) };
      }
    },
  );
  const failures = results.filter(
    (result): result is Extract<StressWalletResolutionResult, { ok: false }> =>
      !result.ok,
  );
  if (failures.length > 0) {
    throw new Error(
      `${failures.length.toString()}/${envNames.length.toString()} stress wallet env vars are unresolvable:\n` +
        failures
          .map((failure) => `  - ${failure.envName}: ${failure.error}`)
          .join("\n"),
    );
  }
  return results.flatMap((result) => (result.ok ? [result.wallet] : []));
};

const validateDistinctStressWallets = (
  wallets: readonly StressWallet[],
): void => {
  const seedSources = new Set<string>();
  const addresses = new Set<string>();
  for (const wallet of wallets) {
    if (seedSources.has(wallet.resolvedWalletSeedPhrase.resolvedFrom)) {
      throw new Error(
        `Duplicate stress wallet seed source ${wallet.resolvedWalletSeedPhrase.resolvedFrom}.`,
      );
    }
    if (addresses.has(wallet.address)) {
      throw new Error(
        `Stress wallet seeds must derive distinct addresses; duplicate address ${wallet.address}.`,
      );
    }
    seedSources.add(wallet.resolvedWalletSeedPhrase.resolvedFrom);
    addresses.add(wallet.address);
  }
};

export const parseE2EL2StressConfig = ({
  endpoint,
  loadModel: rawLoadModel,
  workloadProfile: rawWorkloadProfile,
  mode: rawMode,
  corpusShape: rawCorpusShape,
  corpusPath,
  corpusSliceId,
  targetRateTps: rawTargetRateTps,
  openLoopDurationMs: rawOpenLoopDurationMs,
  openLoopWarmupCount: rawOpenLoopWarmupCount,
  openLoopCooldownCount: rawOpenLoopCooldownCount,
  openLoopMaxInFlight: rawOpenLoopMaxInFlight,
  noOpCalibrationEndpoint,
  requireNoOpCalibration = false,
  noOpCalibrationDurationMs: rawNoOpCalibrationDurationMs,
  aggregateObserverIntervalMs: rawAggregateObserverIntervalMs,
  count: rawCount,
  concurrency: rawConcurrency,
  lovelace: rawLovelace,
  feeHeadroomLovelace: rawFeeHeadroomLovelace,
  walletSeedPhrase,
  walletSeedPhraseEnv = DEFAULT_WALLET_SEED_ENV,
  stressWalletSeedPhraseEnvs = [],
  l2Address,
  runId,
  outDir,
  pollIntervalMs: rawPollIntervalMs,
  pollInitialIntervalMs: rawPollInitialIntervalMs,
  pollMaxIntervalMs: rawPollMaxIntervalMs,
  submitRequestTimeoutMs: rawSubmitRequestTimeoutMs,
  acceptanceTimeoutMs: rawAcceptanceTimeoutMs,
  commitObservationTimeoutMs: rawCommitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    rawFinalityObserverMaxConcurrentRequests,
  maxSubmissionFailures: rawMaxSubmissionFailures,
  network = "Preprod",
  env = process.env,
  allowUnsafeBounds = false,
}: ParseE2EL2StressOptions): E2EL2StressConfig => {
  const loadModel = parseLoadModel(rawLoadModel);
  const workloadProfile = parseWorkloadProfile({
    value: rawWorkloadProfile,
    loadModel,
  });
  const mode = parseMode(rawMode);
  const corpusShape = parseCorpusShape(rawCorpusShape);
  const count = parsePositiveInteger(rawCount, "--count", DEFAULT_COUNT);
  const targetRateTps = parsePositiveNumber(
    rawTargetRateTps,
    "--target-rate-tps",
    DEFAULT_OPEN_LOOP_TARGET_RATE_TPS,
  );
  const openLoopDurationMs = parsePositiveInteger(
    rawOpenLoopDurationMs,
    "--open-loop-duration-ms",
    DEFAULT_OPEN_LOOP_DURATION_MS,
  );
  const openLoopWarmupCount = parseNonNegativeInteger(
    rawOpenLoopWarmupCount,
    "--open-loop-warmup-count",
    0,
  );
  const openLoopCooldownCount = parseNonNegativeInteger(
    rawOpenLoopCooldownCount,
    "--open-loop-cooldown-count",
    0,
  );
  const openLoopMaxInFlight = parsePositiveInteger(
    rawOpenLoopMaxInFlight,
    "--open-loop-max-in-flight",
    DEFAULT_OPEN_LOOP_MAX_IN_FLIGHT,
  );
  const noOpCalibrationDurationMs = parsePositiveInteger(
    rawNoOpCalibrationDurationMs,
    "--no-op-calibration-duration-ms",
    DEFAULT_NO_OP_CALIBRATION_DURATION_MS,
  );
  const aggregateObserverIntervalMs = parsePositiveInteger(
    rawAggregateObserverIntervalMs,
    "--aggregate-observer-interval-ms",
    DEFAULT_AGGREGATE_OBSERVER_INTERVAL_MS,
  );
  const concurrency = parsePositiveInteger(
    rawConcurrency,
    "--concurrency",
    DEFAULT_CONCURRENCY,
  );
  const lovelace = parsePositiveBigInt(
    rawLovelace,
    "--lovelace",
    DEFAULT_LOVELACE,
  );
  const feeHeadroomLovelace = parseNonNegativeBigInt(
    rawFeeHeadroomLovelace,
    "--fee-headroom-lovelace",
    DEFAULT_FEE_HEADROOM_LOVELACE,
  );
  const pollIntervalMs =
    rawPollIntervalMs === undefined || rawPollIntervalMs.trim().length === 0
      ? undefined
      : parsePositiveInteger(
          rawPollIntervalMs,
          "--poll-interval-ms",
          DEFAULT_POLL_INTERVAL_MS,
        );
  const pollInitialIntervalMs = parsePositiveInteger(
    rawPollInitialIntervalMs,
    "--poll-initial-interval-ms",
    DEFAULT_POLL_INITIAL_INTERVAL_MS,
  );
  const pollMaxIntervalMs = parsePositiveInteger(
    rawPollMaxIntervalMs,
    "--poll-max-interval-ms",
    DEFAULT_POLL_MAX_INTERVAL_MS,
  );
  const submitRequestTimeoutMs = parsePositiveInteger(
    rawSubmitRequestTimeoutMs,
    "--submit-request-timeout-ms",
    DEFAULT_SUBMIT_REQUEST_TIMEOUT_MS,
  );
  const acceptanceTimeoutMs = parsePositiveInteger(
    rawAcceptanceTimeoutMs,
    "--acceptance-timeout-ms",
    DEFAULT_ACCEPTANCE_TIMEOUT_MS,
  );
  const commitObservationTimeoutMs = parsePositiveInteger(
    rawCommitObservationTimeoutMs,
    "--commit-observation-timeout-ms",
    DEFAULT_COMMIT_OBSERVATION_TIMEOUT_MS,
  );
  const finalityObserverMaxConcurrentRequests = parsePositiveInteger(
    rawFinalityObserverMaxConcurrentRequests,
    "--finality-observer-max-concurrent-requests",
    DEFAULT_FINALITY_OBSERVER_MAX_CONCURRENT_REQUESTS,
  );
  const maxSubmissionFailures = parseNonNegativeInteger(
    rawMaxSubmissionFailures,
    "--max-submission-failures",
    DEFAULT_MAX_SUBMISSION_FAILURES,
  );
  const normalizedEndpoint = parseNodeEndpoint(
    endpoint ?? defaultMidgardNodeEndpoint(env),
  );
  const resolvedRunId = runId?.trim() || `e2e-run-${timestampForPath()}`;
  const primaryWallet =
    loadModel === "closed-loop-smoke" && mode === "serial-chain"
      ? resolveStressWallet({
          walletSeedPhrase,
          walletSeedPhraseEnv,
          env,
          network,
        })
      : undefined;
  const stressWallets =
    loadModel === "closed-loop-smoke"
      ? resolveStressWallets({
          envNames: stressWalletSeedPhraseEnvs,
          env,
          network,
        })
      : [];
  validateDistinctStressWallets(stressWallets);

  if (count > MAX_DEFAULT_COUNT && !allowUnsafeBounds) {
    throw new Error(
      `--count ${count.toString()} exceeds the default cap ${MAX_DEFAULT_COUNT.toString()}; pass --unsafe-allow-large-stress to make that choice explicit.`,
    );
  }
  if (concurrency > count) {
    throw new Error("--concurrency must be less than or equal to --count.");
  }
  if (concurrency > MAX_DEFAULT_CONCURRENCY && !allowUnsafeBounds) {
    throw new Error(
      `--concurrency ${concurrency.toString()} exceeds the default cap ${MAX_DEFAULT_CONCURRENCY.toString()}; pass --unsafe-allow-large-stress to make that choice explicit.`,
    );
  }
  if (
    loadModel === "closed-loop-smoke" &&
    concurrency > 1 &&
    mode !== "parallel-fanout"
  ) {
    throw new Error(
      "--concurrency > 1 requires --mode parallel-fanout and independent stress wallet seed env vars.",
    );
  }
  if (
    loadModel === "closed-loop-smoke" &&
    mode === "parallel-fanout" &&
    stressWallets.length < concurrency
  ) {
    throw new Error(
      `--mode parallel-fanout requires at least ${concurrency.toString()} independent --stress-wallet-seed-phrase-env values with spendable L2 UTxOs; no submissions were made.`,
    );
  }
  if (loadModel === "open-loop-upper-bound") {
    if (corpusPath === undefined || corpusPath.trim().length === 0) {
      throw new Error(
        "--load-model open-loop-upper-bound requires --tx-corpus with prebuilt canonical CBOR rows.",
      );
    }
    if (
      workloadProfile === "production-end-user" &&
      noOpCalibrationEndpoint === undefined
    ) {
      throw new Error(
        "production-end-user open-loop runs must still provide --no-op-calibration-endpoint before upper-bound claims.",
      );
    }
  }
  if (requireNoOpCalibration && noOpCalibrationEndpoint === undefined) {
    throw new Error(
      "--require-no-op-calibration requires --no-op-calibration-endpoint.",
    );
  }

  return {
    runId: resolvedRunId,
    loadModel,
    workloadProfile,
    mode,
    corpusShape,
    count,
    concurrency,
    lovelace,
    feeHeadroomLovelace,
    nodeEndpoint: normalizedEndpoint,
    ...(corpusPath === undefined || corpusPath.trim().length === 0
      ? {}
      : { corpusPath: corpusPath.trim() }),
    corpusSliceId: corpusSliceId?.trim() || "default",
    targetRateTps,
    openLoopDurationMs,
    openLoopWarmupCount,
    openLoopCooldownCount,
    openLoopMaxInFlight,
    ...(noOpCalibrationEndpoint === undefined ||
    noOpCalibrationEndpoint.trim().length === 0
      ? {}
      : {
          noOpCalibrationEndpoint: parseNodeEndpoint(noOpCalibrationEndpoint),
        }),
    requireNoOpCalibration,
    noOpCalibrationDurationMs,
    aggregateObserverIntervalMs,
    ...(l2Address === undefined || l2Address.trim().length === 0
      ? {}
      : { destinationAddress: parseAddressArgument(l2Address) }),
    pollIntervalMs,
    pollInitialIntervalMs,
    pollMaxIntervalMs,
    submitRequestTimeoutMs,
    acceptanceTimeoutMs,
    commitObservationTimeoutMs,
    finalityObserverMaxConcurrentRequests,
    maxSubmissionFailures,
    outDir: outDir?.trim() || join("logs", resolvedRunId, "stress"),
    network,
    allowUnsafeBounds,
    primaryWallet,
    stressWallets,
  };
};

const rawArtifactConfig = (config: E2EL2StressConfig) => ({
  schemaVersion: E2E_L2_STRESS_CONFIG_SCHEMA_VERSION,
  runId: config.runId,
  loadModel: config.loadModel,
  workloadProfile: config.workloadProfile,
  corpusShape: config.corpusShape,
  mode: config.mode,
  measurementPolicy: measurementPolicyForConfig(config),
  count: config.count,
  concurrency: config.concurrency,
  lovelace: config.lovelace.toString(10),
  feeHeadroomLovelace: config.feeHeadroomLovelace.toString(10),
  nodeEndpoint: config.nodeEndpoint,
  corpusPath: config.corpusPath ?? null,
  corpusSliceId: config.corpusSliceId,
  targetRateTps: config.targetRateTps,
  openLoopDurationMs: config.openLoopDurationMs,
  openLoopWarmupCount: config.openLoopWarmupCount,
  openLoopCooldownCount: config.openLoopCooldownCount,
  openLoopMaxInFlight: config.openLoopMaxInFlight,
  noOpCalibrationEndpoint: config.noOpCalibrationEndpoint ?? null,
  requireNoOpCalibration: config.requireNoOpCalibration,
  noOpCalibrationDurationMs: config.noOpCalibrationDurationMs,
  aggregateObserverIntervalMs: config.aggregateObserverIntervalMs,
  destination:
    config.destinationAddress === undefined
      ? { mode: "self" }
      : { mode: "explicit", address: config.destinationAddress },
  pollIntervalMs: config.pollIntervalMs,
  pollInitialIntervalMs: config.pollInitialIntervalMs,
  pollMaxIntervalMs: config.pollMaxIntervalMs,
  submitRequestTimeoutMs: config.submitRequestTimeoutMs,
  acceptanceTimeoutMs: config.acceptanceTimeoutMs,
  commitObservationTimeoutMs: config.commitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    config.finalityObserverMaxConcurrentRequests,
  maxSubmissionFailures: config.maxSubmissionFailures,
  network: config.network,
  allowUnsafeBounds: config.allowUnsafeBounds,
  wallets:
    config.loadModel === "open-loop-upper-bound"
      ? []
      : config.mode === "parallel-fanout"
        ? config.stressWallets.map((wallet) => ({
            seedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
            address: wallet.address,
          }))
        : [
            {
              seedSource:
                requirePrimaryWallet(config).resolvedWalletSeedPhrase
                  .resolvedFrom,
              address: requirePrimaryWallet(config).address,
            },
          ],
});

export type E2EL2StressConfigArtifactV1 = ReturnType<typeof rawArtifactConfig>;

const artifactConfig = (
  config: E2EL2StressConfig,
): E2EL2StressConfigArtifactV1 =>
  parseE2EL2StressConfigArtifactV1(rawArtifactConfig(config));

const assertMeasurementPolicyV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "loadModel",
    "workloadProfile",
    "syntheticVsProduction",
    "advanceOn",
    "primaryStageMetric",
    "finalityObservation",
    "submissionWindowExcludesCommitDrain",
    "fullFinalityRequiresDrainProof",
  ]);
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  oneOf(input.syntheticVsProduction, `${label}.syntheticVsProduction`, [
    "synthetic_admission_diagnostic",
    "production_end_user_path",
  ]);
  oneOf(input.advanceOn, `${label}.advanceOn`, [
    "accepted",
    "scheduled_submit",
  ]);
  oneOf(input.primaryStageMetric, `${label}.primaryStageMetric`, [
    "metrics.l2Admission.perSecond",
    "metrics.durableAdmission.perSecond",
  ]);
  oneOf(input.finalityObservation, `${label}.finalityObservation`, [
    "post-submit-bounded",
    "aggregate-window",
  ]);
  exactLiteral(
    input.submissionWindowExcludesCommitDrain,
    `${label}.submissionWindowExcludesCommitDrain`,
    true,
  );
  exactLiteral(
    input.fullFinalityRequiresDrainProof,
    `${label}.fullFinalityRequiresDrainProof`,
    true,
  );
};

const parseDecimalString = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${label} must be an unsigned canonical decimal string`);
  }
  return parsed;
};

const assertStressWalletArtifactV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, ["seedSource", "address"]);
  nonEmptyString(input.seedSource, `${label}.seedSource`);
  nonEmptyString(input.address, `${label}.address`);
};

export function parseE2EL2StressConfigArtifactV1(
  value: unknown,
): E2EL2StressConfigArtifactV1 {
  const label = "E2E L2 stress config";
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "runId",
      "loadModel",
      "workloadProfile",
      "corpusShape",
      "mode",
      "measurementPolicy",
      "count",
      "concurrency",
      "lovelace",
      "feeHeadroomLovelace",
      "nodeEndpoint",
      "corpusPath",
      "corpusSliceId",
      "targetRateTps",
      "openLoopDurationMs",
      "openLoopWarmupCount",
      "openLoopCooldownCount",
      "openLoopMaxInFlight",
      "noOpCalibrationEndpoint",
      "requireNoOpCalibration",
      "noOpCalibrationDurationMs",
      "aggregateObserverIntervalMs",
      "destination",
      "pollInitialIntervalMs",
      "pollMaxIntervalMs",
      "submitRequestTimeoutMs",
      "acceptanceTimeoutMs",
      "commitObservationTimeoutMs",
      "finalityObserverMaxConcurrentRequests",
      "maxSubmissionFailures",
      "network",
      "allowUnsafeBounds",
      "wallets",
    ],
    ["pollIntervalMs"],
  );
  if (input.schemaVersion !== E2E_L2_STRESS_CONFIG_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_L2_STRESS_CONFIG_SCHEMA_VERSION}`,
    );
  }
  nonEmptyString(input.runId, `${label}.runId`);
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  oneOf(input.corpusShape, `${label}.corpusShape`, [
    "fanout",
    "chain",
    "mixed",
  ]);
  oneOf(input.mode, `${label}.mode`, ["serial-chain", "parallel-fanout"]);
  assertMeasurementPolicyV1(
    input.measurementPolicy,
    `${label}.measurementPolicy`,
  );
  positiveInteger(input.count, `${label}.count`);
  positiveInteger(input.concurrency, `${label}.concurrency`);
  parseDecimalString(input.lovelace, `${label}.lovelace`);
  parseDecimalString(input.feeHeadroomLovelace, `${label}.feeHeadroomLovelace`);
  nonEmptyString(input.nodeEndpoint, `${label}.nodeEndpoint`);
  nullableNonEmptyString(input.corpusPath, `${label}.corpusPath`);
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.openLoopDurationMs, `${label}.openLoopDurationMs`);
  nonNegativeInteger(input.openLoopWarmupCount, `${label}.openLoopWarmupCount`);
  nonNegativeInteger(
    input.openLoopCooldownCount,
    `${label}.openLoopCooldownCount`,
  );
  positiveInteger(input.openLoopMaxInFlight, `${label}.openLoopMaxInFlight`);
  nullableNonEmptyString(
    input.noOpCalibrationEndpoint,
    `${label}.noOpCalibrationEndpoint`,
  );
  booleanValue(input.requireNoOpCalibration, `${label}.requireNoOpCalibration`);
  positiveInteger(
    input.noOpCalibrationDurationMs,
    `${label}.noOpCalibrationDurationMs`,
  );
  positiveInteger(
    input.aggregateObserverIntervalMs,
    `${label}.aggregateObserverIntervalMs`,
  );
  const destination = exactRecord(
    input.destination,
    `${label}.destination`,
    ["mode"],
    ["address"],
  );
  const destinationMode = oneOf(destination.mode, `${label}.destination.mode`, [
    "self",
    "explicit",
  ]);
  if (destinationMode === "self") {
    exactRecord(input.destination, `${label}.destination`, ["mode"]);
  } else {
    const explicitDestination = exactRecord(
      input.destination,
      `${label}.destination`,
      ["mode", "address"],
    );
    nonEmptyString(explicitDestination.address, `${label}.destination.address`);
  }
  if (input.pollIntervalMs !== undefined) {
    positiveInteger(input.pollIntervalMs, `${label}.pollIntervalMs`);
  }
  positiveInteger(
    input.pollInitialIntervalMs,
    `${label}.pollInitialIntervalMs`,
  );
  positiveInteger(input.pollMaxIntervalMs, `${label}.pollMaxIntervalMs`);
  positiveInteger(
    input.submitRequestTimeoutMs,
    `${label}.submitRequestTimeoutMs`,
  );
  positiveInteger(input.acceptanceTimeoutMs, `${label}.acceptanceTimeoutMs`);
  positiveInteger(
    input.commitObservationTimeoutMs,
    `${label}.commitObservationTimeoutMs`,
  );
  positiveInteger(
    input.finalityObserverMaxConcurrentRequests,
    `${label}.finalityObserverMaxConcurrentRequests`,
  );
  nonNegativeInteger(
    input.maxSubmissionFailures,
    `${label}.maxSubmissionFailures`,
  );
  oneOf(input.network, `${label}.network`, [
    "Mainnet",
    "Preview",
    "Preprod",
    "Custom",
  ]);
  booleanValue(input.allowUnsafeBounds, `${label}.allowUnsafeBounds`);
  arrayOf(input.wallets, `${label}.wallets`, (entry, entryLabel) => {
    assertStressWalletArtifactV1(entry, entryLabel);
    return entry;
  });
  return value as E2EL2StressConfigArtifactV1;
}

const parseLowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const assertStressMetricWindowV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "status",
    "count",
    "startedAt",
    "finishedAt",
    "durationMs",
    "perSecond",
    "source",
    "precision",
    "missingCount",
    "notes",
  ]);
  const status = oneOf(input.status, `${label}.status`, [
    "complete",
    "partial",
    "unavailable",
  ]);
  const count = nonNegativeInteger(input.count, `${label}.count`);
  const startedAt =
    input.startedAt === null
      ? null
      : isoTimestamp(input.startedAt, `${label}.startedAt`);
  const finishedAt =
    input.finishedAt === null
      ? null
      : isoTimestamp(input.finishedAt, `${label}.finishedAt`);
  const durationMs = nullableNonNegativeNumber(
    input.durationMs,
    `${label}.durationMs`,
  );
  const perSecond = nullableNonNegativeNumber(
    input.perSecond,
    `${label}.perSecond`,
  );
  nonEmptyString(input.source, `${label}.source`);
  oneOf(input.precision, `${label}.precision`, [
    "db_timestamp",
    "observer_timestamp",
    "artifact_timestamp",
  ]);
  const missingCount = nonNegativeInteger(
    input.missingCount,
    `${label}.missingCount`,
  );
  const notes = stringArray(input.notes, `${label}.notes`);
  const hasCompleteRange = startedAt !== null && finishedAt !== null;
  const elapsedMs = hasCompleteRange
    ? Date.parse(finishedAt) - Date.parse(startedAt)
    : null;
  const expectedPerSecond =
    elapsedMs === null || elapsedMs <= 0
      ? null
      : roundMetric(count / (elapsedMs / 1_000));
  const unavailableIsCanonical =
    status === "unavailable" &&
    count === 0 &&
    startedAt === null &&
    finishedAt === null &&
    durationMs === null &&
    perSecond === null;
  const observedIsCanonical =
    status !== "unavailable" &&
    count > 0 &&
    ((hasCompleteRange &&
      elapsedMs! >= 0 &&
      durationMs === elapsedMs &&
      perSecond === expectedPerSecond &&
      (status === "complete" ? missingCount === 0 : missingCount > 0)) ||
      (!hasCompleteRange &&
        status === "partial" &&
        durationMs === null &&
        perSecond === null));
  if (
    (!unavailableIsCanonical && !observedIsCanonical) ||
    new Set(notes).size !== notes.length
  ) {
    throw new Error(`${label} status, window, rate, or notes are inconsistent`);
  }
};

const assertStressMetricsV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "clientSubmission",
    "durableAdmission",
    "l2Admission",
    "l1Commit",
    "immutableObservation",
    "fullFinality",
  ]);
  assertStressMetricWindowV1(
    input.clientSubmission,
    `${label}.clientSubmission`,
  );
  assertStressMetricWindowV1(
    input.durableAdmission,
    `${label}.durableAdmission`,
  );
  assertStressMetricWindowV1(input.l2Admission, `${label}.l2Admission`);
  const l1Commit = exactRecord(input.l1Commit, `${label}.l1Commit`, [
    "headers",
    "l2Transactions",
  ]);
  assertStressMetricWindowV1(l1Commit.headers, `${label}.l1Commit.headers`);
  assertStressMetricWindowV1(
    l1Commit.l2Transactions,
    `${label}.l1Commit.l2Transactions`,
  );
  assertStressMetricWindowV1(
    input.immutableObservation,
    `${label}.immutableObservation`,
  );
  assertStressMetricWindowV1(input.fullFinality, `${label}.fullFinality`);
};

const assertStagePercentilesV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "p50Ms",
    "p95Ms",
    "p99Ms",
    "sampleCount",
  ]);
  nullableNonNegativeNumber(input.p50Ms, `${label}.p50Ms`);
  nullableNonNegativeNumber(input.p95Ms, `${label}.p95Ms`);
  nullableNonNegativeNumber(input.p99Ms, `${label}.p99Ms`);
  nonNegativeInteger(input.sampleCount, `${label}.sampleCount`);
};

const assertSteadyStateStageV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "stage",
    "offeredCount",
    "rawCount",
    "trimmedCount",
    "rawPerSecond",
    "steadyStatePerSecond",
    "latency",
    "windowTrim",
    "precision",
    "notes",
  ]);
  nonEmptyString(input.stage, `${label}.stage`);
  nonNegativeInteger(input.offeredCount, `${label}.offeredCount`);
  nonNegativeInteger(input.rawCount, `${label}.rawCount`);
  nonNegativeInteger(input.trimmedCount, `${label}.trimmedCount`);
  nullableNonNegativeNumber(input.rawPerSecond, `${label}.rawPerSecond`);
  nullableNonNegativeNumber(
    input.steadyStatePerSecond,
    `${label}.steadyStatePerSecond`,
  );
  assertStagePercentilesV1(input.latency, `${label}.latency`);
  const windowTrim = exactRecord(input.windowTrim, `${label}.windowTrim`, [
    "discardedHeadMs",
    "discardedTailMs",
  ]);
  nonNegativeNumber(
    windowTrim.discardedHeadMs,
    `${label}.windowTrim.discardedHeadMs`,
  );
  nonNegativeNumber(
    windowTrim.discardedTailMs,
    `${label}.windowTrim.discardedTailMs`,
  );
  exactLiteral(input.precision, `${label}.precision`, "db_timestamp");
  stringArray(input.notes, `${label}.notes`);
};

const assertEnvironmentFingerprintV1 = (
  value: unknown,
  label: string,
): void => {
  const input = exactRecord(value, label, [
    "schemaVersion",
    "gitSha",
    "imageDigests",
    "hostCpu",
    "hostRamBytes",
    "hostname",
    "loadGenCoHosted",
    "loadGeneratorPlacement",
    "clockOffsetMs",
    "calibrationProofRef",
    "configProfileHash",
    "fixedKnobs",
    "capturedAt",
    "notes",
  ]);
  exactLiteral(input.schemaVersion, `${label}.schemaVersion`, 1);
  nullableNonEmptyString(input.gitSha, `${label}.gitSha`);
  const imageDigests = exactRecord(
    input.imageDigests,
    `${label}.imageDigests`,
    ["midgardNode", "postgres"],
  );
  nullableNonEmptyString(
    imageDigests.midgardNode,
    `${label}.imageDigests.midgardNode`,
  );
  nullableNonEmptyString(
    imageDigests.postgres,
    `${label}.imageDigests.postgres`,
  );
  const hostCpu = exactRecord(input.hostCpu, `${label}.hostCpu`, [
    "model",
    "count",
    "speedMhz",
  ]);
  nullableNonEmptyString(hostCpu.model, `${label}.hostCpu.model`);
  nonNegativeInteger(hostCpu.count, `${label}.hostCpu.count`);
  nullableNonNegativeNumber(hostCpu.speedMhz, `${label}.hostCpu.speedMhz`);
  nonNegativeNumber(input.hostRamBytes, `${label}.hostRamBytes`);
  nonEmptyString(input.hostname, `${label}.hostname`);
  if (input.loadGenCoHosted !== null) {
    booleanValue(input.loadGenCoHosted, `${label}.loadGenCoHosted`);
  }
  oneOf(input.loadGeneratorPlacement, `${label}.loadGeneratorPlacement`, [
    "separate-host",
    "separate-container",
    "node-container",
    "unknown",
  ]);
  if (input.clockOffsetMs !== null) {
    finiteNumber(input.clockOffsetMs, `${label}.clockOffsetMs`);
  }
  nullableNonEmptyString(
    input.calibrationProofRef,
    `${label}.calibrationProofRef`,
  );
  parseLowerHex64(input.configProfileHash, `${label}.configProfileHash`);
  const fixedKnobs = exactRecord(input.fixedKnobs, `${label}.fixedKnobs`, [
    "nodePostgresPoolMaxConnections",
    "validationBatchHardCap",
    "validationMinBatch",
    "validationPhaseAMaxEffectiveConcurrency",
  ]);
  positiveInteger(
    fixedKnobs.nodePostgresPoolMaxConnections,
    `${label}.fixedKnobs.nodePostgresPoolMaxConnections`,
  );
  positiveInteger(
    fixedKnobs.validationBatchHardCap,
    `${label}.fixedKnobs.validationBatchHardCap`,
  );
  positiveInteger(
    fixedKnobs.validationMinBatch,
    `${label}.fixedKnobs.validationMinBatch`,
  );
  positiveInteger(
    fixedKnobs.validationPhaseAMaxEffectiveConcurrency,
    `${label}.fixedKnobs.validationPhaseAMaxEffectiveConcurrency`,
  );
  isoTimestamp(input.capturedAt, `${label}.capturedAt`);
  stringArray(input.notes, `${label}.notes`);
};

const assertGroundTruthMetricsV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "schemaVersion",
    "window",
    "stages",
    "fingerprint",
  ]);
  exactLiteral(input.schemaVersion, `${label}.schemaVersion`, 1);
  const window = exactRecord(input.window, `${label}.window`, [
    "start",
    "end",
    "trimFraction",
  ]);
  isoTimestamp(window.start, `${label}.window.start`);
  isoTimestamp(window.end, `${label}.window.end`);
  nonNegativeNumber(window.trimFraction, `${label}.window.trimFraction`);
  const stages = exactRecord(input.stages, `${label}.stages`, [
    "admission",
    "validationStart",
    "validationTerminal",
    "mempoolPersist",
    "l1CommitHeader",
    "l1CommitConfirm",
    "immutableObservation",
    "fullFinality",
  ]);
  for (const stage of [
    "admission",
    "validationStart",
    "validationTerminal",
    "mempoolPersist",
    "l1CommitHeader",
    "l1CommitConfirm",
    "immutableObservation",
    "fullFinality",
  ] as const) {
    assertSteadyStateStageV1(stages[stage], `${label}.stages.${stage}`);
  }
  assertEnvironmentFingerprintV1(input.fingerprint, `${label}.fingerprint`);
};

const assertCorpusRowV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
    "canonicalCborByteLength",
    "senderWalletId",
    "selectedInputOutref",
    "outputOutrefs",
    "planShape",
    "parentTxHash",
    "corpusSliceId",
  ]);
  parseLowerHex64(input.txHash, `${label}.txHash`);
  const cborHex = nonEmptyString(
    input.canonicalCborHex,
    `${label}.canonicalCborHex`,
  );
  if (!/^(?:[0-9a-f]{2})+$/u.test(cborHex)) {
    throw new Error(`${label}.canonicalCborHex must be lowercase whole bytes`);
  }
  parseLowerHex64(input.canonicalCborSha256, `${label}.canonicalCborSha256`);
  positiveInteger(
    input.canonicalCborByteLength,
    `${label}.canonicalCborByteLength`,
  );
  nonEmptyString(input.senderWalletId, `${label}.senderWalletId`);
  nonEmptyString(input.selectedInputOutref, `${label}.selectedInputOutref`);
  stringArray(input.outputOutrefs, `${label}.outputOutrefs`);
  oneOf(input.planShape, `${label}.planShape`, ["fanout", "chain", "mixed"]);
  if (input.parentTxHash !== null) {
    parseLowerHex64(input.parentTxHash, `${label}.parentTxHash`);
  }
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
};

const assertCorpusPlanV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "rows",
    "requiredTransactionCount",
    "selectedTransactionCount",
    "corpusShape",
    "corpusSliceId",
  ]);
  arrayOf(input.rows, `${label}.rows`, (entry, entryLabel) => {
    assertCorpusRowV1(entry, entryLabel);
    return entry;
  });
  nonNegativeInteger(
    input.requiredTransactionCount,
    `${label}.requiredTransactionCount`,
  );
  nonNegativeInteger(
    input.selectedTransactionCount,
    `${label}.selectedTransactionCount`,
  );
  oneOf(input.corpusShape, `${label}.corpusShape`, [
    "fanout",
    "chain",
    "mixed",
  ]);
  nonEmptyString(input.corpusSliceId, `${label}.corpusSliceId`);
};

const assertScheduleSlipV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, ["p50", "p95", "p99", "max"]);
  nonNegativeNumber(input.p50, `${label}.p50`);
  nonNegativeNumber(input.p95, `${label}.p95`);
  nonNegativeNumber(input.p99, `${label}.p99`);
  nonNegativeNumber(input.max, `${label}.max`);
};

const assertOpenLoopSubmitSummaryV1 = (
  value: unknown,
  label: string,
  calibration: boolean,
): void => {
  const baseKeys = [
    "offeredCount",
    "submittedCount",
    "failedCount",
    "targetRateTps",
    "maxInFlight",
    "maxObservedInFlight",
    "startedAtIso",
    "finishedAtIso",
    "durationMs",
    "achievedRateTps",
    "submittedOfferedRatio",
    "scheduleSlipMs",
  ] as const;
  const calibrationKeys = [
    "endpoint",
    "minRequiredRateTps",
    "p95ScheduleSlipLimitMs",
    "p99ScheduleSlipLimitMs",
    "passed",
    "cpuUserMicros",
    "cpuSystemMicros",
    "notes",
  ] as const;
  const input = exactRecord(
    value,
    label,
    calibration ? [...baseKeys, ...calibrationKeys] : baseKeys,
    calibration ? ["eventLoopUtilization"] : [],
  );
  nonNegativeInteger(input.offeredCount, `${label}.offeredCount`);
  nonNegativeInteger(input.submittedCount, `${label}.submittedCount`);
  nonNegativeInteger(input.failedCount, `${label}.failedCount`);
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.maxInFlight, `${label}.maxInFlight`);
  nonNegativeInteger(input.maxObservedInFlight, `${label}.maxObservedInFlight`);
  isoTimestamp(input.startedAtIso, `${label}.startedAtIso`);
  isoTimestamp(input.finishedAtIso, `${label}.finishedAtIso`);
  nonNegativeNumber(input.durationMs, `${label}.durationMs`);
  nonNegativeNumber(input.achievedRateTps, `${label}.achievedRateTps`);
  nonNegativeNumber(
    input.submittedOfferedRatio,
    `${label}.submittedOfferedRatio`,
  );
  assertScheduleSlipV1(input.scheduleSlipMs, `${label}.scheduleSlipMs`);
  if (calibration) {
    nonEmptyString(input.endpoint, `${label}.endpoint`);
    nonNegativeNumber(input.minRequiredRateTps, `${label}.minRequiredRateTps`);
    nonNegativeNumber(
      input.p95ScheduleSlipLimitMs,
      `${label}.p95ScheduleSlipLimitMs`,
    );
    nonNegativeNumber(
      input.p99ScheduleSlipLimitMs,
      `${label}.p99ScheduleSlipLimitMs`,
    );
    booleanValue(input.passed, `${label}.passed`);
    if (input.eventLoopUtilization !== undefined) {
      nonNegativeNumber(
        input.eventLoopUtilization,
        `${label}.eventLoopUtilization`,
      );
    }
    nonNegativeNumber(input.cpuUserMicros, `${label}.cpuUserMicros`);
    nonNegativeNumber(input.cpuSystemMicros, `${label}.cpuSystemMicros`);
    stringArray(input.notes, `${label}.notes`);
  }
};

const assertOpenLoopPlacementV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "processPid",
    "cwd",
    "insideMidgardNodeProcess",
    "insideMidgardNodeContainer",
    "validForUpperBoundClaim",
    "notes",
  ]);
  positiveInteger(input.processPid, `${label}.processPid`);
  nonEmptyString(input.cwd, `${label}.cwd`);
  booleanValue(
    input.insideMidgardNodeProcess,
    `${label}.insideMidgardNodeProcess`,
  );
  booleanValue(
    input.insideMidgardNodeContainer,
    `${label}.insideMidgardNodeContainer`,
  );
  booleanValue(
    input.validForUpperBoundClaim,
    `${label}.validForUpperBoundClaim`,
  );
  stringArray(input.notes, `${label}.notes`);
};

const assertOpenLoopSummaryV1 = (value: unknown, label: string): void => {
  const input = exactRecord(
    value,
    label,
    [
      "targetRateTps",
      "durationMs",
      "maxInFlight",
      "corpus",
      "submission",
      "placement",
    ],
    ["calibration"],
  );
  nonNegativeNumber(input.targetRateTps, `${label}.targetRateTps`);
  positiveInteger(input.durationMs, `${label}.durationMs`);
  positiveInteger(input.maxInFlight, `${label}.maxInFlight`);
  assertCorpusPlanV1(input.corpus, `${label}.corpus`);
  assertOpenLoopSubmitSummaryV1(input.submission, `${label}.submission`, false);
  if (input.calibration !== undefined) {
    assertOpenLoopSubmitSummaryV1(
      input.calibration,
      `${label}.calibration`,
      true,
    );
  }
  assertOpenLoopPlacementV1(input.placement, `${label}.placement`);
};

const assertStressTransactionV1 = (value: unknown, label: string): void => {
  const input = exactRecord(value, label, [
    "index",
    "phase",
    "txHash",
    "senderAddress",
    "destinationAddress",
    "selectedInputs",
    "submission",
    "acceptance",
    "finality",
    "workerIndex",
    "walletSeedSource",
  ]);
  nonNegativeInteger(input.index, `${label}.index`);
  exactLiteral(input.phase, `${label}.phase`, "stress");
  if (input.txHash !== null) {
    parseLowerHex64(input.txHash, `${label}.txHash`);
  }
  nonEmptyString(input.senderAddress, `${label}.senderAddress`);
  nonEmptyString(input.destinationAddress, `${label}.destinationAddress`);
  stringArray(input.selectedInputs, `${label}.selectedInputs`);
  const submission = exactRecord(
    input.submission,
    `${label}.submission`,
    ["status", "submittedAt"],
    ["durationMs", "error"],
  );
  oneOf(submission.status, `${label}.submission.status`, [
    "submitted",
    "failed",
  ]);
  if (submission.submittedAt !== null) {
    isoTimestamp(submission.submittedAt, `${label}.submission.submittedAt`);
  }
  if (submission.durationMs !== undefined) {
    nonNegativeNumber(submission.durationMs, `${label}.submission.durationMs`);
  }
  if (submission.error !== undefined) {
    nonEmptyString(submission.error, `${label}.submission.error`);
  }
  const acceptance = exactRecord(
    input.acceptance,
    `${label}.acceptance`,
    ["status"],
    ["acceptedAt", "durationMs", "error"],
  );
  oneOf(acceptance.status, `${label}.acceptance.status`, [
    "accepted",
    "rejected",
    "timeout",
    "not_observed",
    "not_submitted",
  ]);
  if (acceptance.acceptedAt !== undefined) {
    isoTimestamp(acceptance.acceptedAt, `${label}.acceptance.acceptedAt`);
  }
  if (acceptance.durationMs !== undefined) {
    nonNegativeNumber(acceptance.durationMs, `${label}.acceptance.durationMs`);
  }
  if (acceptance.error !== undefined) {
    nonEmptyString(acceptance.error, `${label}.acceptance.error`);
  }
  const finality = exactRecord(
    input.finality,
    `${label}.finality`,
    ["status"],
    ["committedAt", "durationMs", "error"],
  );
  oneOf(finality.status, `${label}.finality.status`, [
    "committed",
    "rejected",
    "timeout",
    "not_observed",
  ]);
  if (finality.committedAt !== undefined) {
    isoTimestamp(finality.committedAt, `${label}.finality.committedAt`);
  }
  if (finality.durationMs !== undefined) {
    nonNegativeNumber(finality.durationMs, `${label}.finality.durationMs`);
  }
  if (finality.error !== undefined) {
    nonEmptyString(finality.error, `${label}.finality.error`);
  }
  nonNegativeInteger(input.workerIndex, `${label}.workerIndex`);
  nonEmptyString(input.walletSeedSource, `${label}.walletSeedSource`);
};

export function parseE2EL2StressSummaryV1(value: unknown): E2EL2StressSummary {
  const label = "E2E L2 stress summary";
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "runId",
      "status",
      "loadModel",
      "workloadProfile",
      "classification",
      "rateSemantics",
      "burstCycleRatePerSecond",
      "mode",
      "measurementPolicy",
      "requestedCount",
      "notStartedCount",
      "submittedCount",
      "submissionFailedCount",
      "acceptedCount",
      "acceptanceNotObservedCount",
      "acceptanceTimedOutCount",
      "finalityTimedOutCount",
      "observedCommittedCount",
      "unknownFinalityCount",
      "rejectedCount",
      "concurrency",
      "finalityObserver",
      "startedAt",
      "submissionFinishedAt",
      "finishedAt",
      "submissionDurationMs",
      "durationMs",
      "metrics",
      "latencyMs",
      "artifactPaths",
      "transactions",
    ],
    [
      "interruptedReason",
      "corpusShape",
      "openLoop",
      "groundTruth",
      "fingerprint",
    ],
  );
  if (input.schemaVersion !== E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION}`,
    );
  }
  nonEmptyString(input.runId, `${label}.runId`);
  oneOf(input.status, `${label}.status`, ["completed", "interrupted"]);
  if (input.interruptedReason !== undefined) {
    nonEmptyString(input.interruptedReason, `${label}.interruptedReason`);
  }
  oneOf(input.loadModel, `${label}.loadModel`, [
    "closed-loop-smoke",
    "open-loop-upper-bound",
  ]);
  oneOf(input.workloadProfile, `${label}.workloadProfile`, [
    "synthetic-admission",
    "production-end-user",
  ]);
  if (input.corpusShape !== undefined) {
    oneOf(input.corpusShape, `${label}.corpusShape`, [
      "fanout",
      "chain",
      "mixed",
    ]);
  }
  oneOf(input.classification, `${label}.classification`, [
    "closed_loop_smoke",
    "full_pipeline_sustained",
    "ingress_ok_commit_failed",
    "admission_bottleneck",
    "validation_bottleneck",
    "commit_planner_bottleneck",
    "da_bottleneck",
    "merge_bottleneck",
    "provider_bottleneck",
    "observer_overloaded",
    "client_overloaded",
    "corpus_exhausted",
    "duplicate_submission",
  ]);
  oneOf(input.rateSemantics, `${label}.rateSemantics`, [
    "burst_cycle_rate",
    "offered_tps_uncalibrated",
  ]);
  nullableNonNegativeNumber(
    input.burstCycleRatePerSecond,
    `${label}.burstCycleRatePerSecond`,
  );
  oneOf(input.mode, `${label}.mode`, ["serial-chain", "parallel-fanout"]);
  assertMeasurementPolicyV1(
    input.measurementPolicy,
    `${label}.measurementPolicy`,
  );
  if (input.openLoop !== undefined) {
    assertOpenLoopSummaryV1(input.openLoop, `${label}.openLoop`);
  }
  for (const countKey of [
    "requestedCount",
    "notStartedCount",
    "submittedCount",
    "submissionFailedCount",
    "acceptedCount",
    "acceptanceNotObservedCount",
    "acceptanceTimedOutCount",
    "finalityTimedOutCount",
    "observedCommittedCount",
    "unknownFinalityCount",
    "rejectedCount",
  ] as const) {
    nonNegativeInteger(input[countKey], `${label}.${countKey}`);
  }
  positiveInteger(input.concurrency, `${label}.concurrency`);
  const finalityObserver = exactRecord(
    input.finalityObserver,
    `${label}.finalityObserver`,
    [
      "mode",
      "maxConcurrentRequests",
      "maxObservedConcurrentRequests",
      "observedTransactionCount",
      "pollRequestCount",
      "batchCount",
      "errorCount",
    ],
  );
  oneOf(finalityObserver.mode, `${label}.finalityObserver.mode`, [
    "post-submit-bounded",
    "aggregate-window",
  ]);
  for (const countKey of [
    "maxConcurrentRequests",
    "maxObservedConcurrentRequests",
    "observedTransactionCount",
    "pollRequestCount",
    "batchCount",
    "errorCount",
  ] as const) {
    nonNegativeInteger(
      finalityObserver[countKey],
      `${label}.finalityObserver.${countKey}`,
    );
  }
  isoTimestamp(input.startedAt, `${label}.startedAt`);
  isoTimestamp(input.submissionFinishedAt, `${label}.submissionFinishedAt`);
  isoTimestamp(input.finishedAt, `${label}.finishedAt`);
  nonNegativeNumber(
    input.submissionDurationMs,
    `${label}.submissionDurationMs`,
  );
  nonNegativeNumber(input.durationMs, `${label}.durationMs`);
  assertStressMetricsV1(input.metrics, `${label}.metrics`);
  if (input.groundTruth !== undefined) {
    assertGroundTruthMetricsV1(input.groundTruth, `${label}.groundTruth`);
  }
  if (input.fingerprint !== undefined) {
    assertEnvironmentFingerprintV1(input.fingerprint, `${label}.fingerprint`);
  }
  const latency = exactRecord(input.latencyMs, `${label}.latencyMs`, [
    "submitP50",
    "submitP95",
    "acceptanceP50",
    "acceptanceP95",
    "commitP50",
    "commitP95",
  ]);
  for (const key of [
    "submitP50",
    "submitP95",
    "acceptanceP50",
    "acceptanceP95",
    "commitP50",
    "commitP95",
  ] as const) {
    nonNegativeNumber(latency[key], `${label}.latencyMs.${key}`);
  }
  const artifactPaths = exactRecord(
    input.artifactPaths,
    `${label}.artifactPaths`,
    ["configJson", "eventsNdjson", "summaryJson", "summaryMarkdown"],
    [
      "engineReportJson",
      "engineEventsNdjson",
      "submitRecordsNdjson",
      "noOpCalibrationJson",
    ],
  );
  for (const key of [
    "configJson",
    "eventsNdjson",
    "summaryJson",
    "summaryMarkdown",
  ] as const) {
    nonEmptyString(artifactPaths[key], `${label}.artifactPaths.${key}`);
  }
  for (const key of [
    "engineReportJson",
    "engineEventsNdjson",
    "submitRecordsNdjson",
    "noOpCalibrationJson",
  ] as const) {
    if (artifactPaths[key] !== undefined) {
      nonEmptyString(artifactPaths[key], `${label}.artifactPaths.${key}`);
    }
  }
  arrayOf(input.transactions, `${label}.transactions`, (entry, entryLabel) => {
    assertStressTransactionV1(entry, entryLabel);
    return entry;
  });
  const parsed = value as E2EL2StressSummary;
  const expectedPolicy = measurementPolicyForConfig({
    loadModel: parsed.loadModel,
    workloadProfile: parsed.workloadProfile,
  });
  const startedAtMs = Date.parse(parsed.startedAt);
  const submissionFinishedAtMs = Date.parse(parsed.submissionFinishedAt);
  const finishedAtMs = Date.parse(parsed.finishedAt);
  const expectedSubmissionDurationMs = submissionFinishedAtMs - startedAtMs;
  const expectedDurationMs = finishedAtMs - startedAtMs;
  const transactionIndexes = parsed.transactions.map(
    (transaction) => transaction.index,
  );
  const transactionHashes = parsed.transactions.flatMap((transaction) =>
    transaction.txHash === null ? [] : [transaction.txHash],
  );
  const expectedSubmittedCount = parsed.transactions.filter(
    (transaction) => transaction.submission.status === "submitted",
  ).length;
  const expectedSubmissionFailedCount = parsed.transactions.filter(
    (transaction) => transaction.submission.status === "failed",
  ).length;
  const expectedAcceptedCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "accepted",
  ).length;
  const expectedAcceptanceNotObservedCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "not_observed",
  ).length;
  const expectedAcceptanceTimedOutCount = parsed.transactions.filter(
    (transaction) => transaction.acceptance.status === "timeout",
  ).length;
  const expectedFinalityTimedOutCount = parsed.transactions.filter(
    (transaction) => transaction.finality.status === "timeout",
  ).length;
  const expectedObservedCommittedCount = parsed.transactions.filter(
    (transaction) => transaction.finality.status === "committed",
  ).length;
  const expectedUnknownFinalityCount = parsed.transactions.filter(
    (transaction) =>
      transaction.acceptance.status === "accepted" &&
      transaction.finality.status === "not_observed",
  ).length;
  const expectedRejectedCount = parsed.transactions.filter(
    (transaction) =>
      transaction.acceptance.status === "rejected" ||
      transaction.finality.status === "rejected",
  ).length;
  const expectedNotStartedCount =
    parsed.requestedCount - parsed.transactions.length;
  const statusReasonIsCanonical =
    (parsed.status === "completed" && parsed.interruptedReason === undefined) ||
    (parsed.status === "interrupted" && parsed.interruptedReason !== undefined);
  const loadModelLanguageIsCanonical =
    (parsed.loadModel === "closed-loop-smoke" &&
      parsed.openLoop === undefined &&
      parsed.corpusShape === undefined &&
      parsed.classification === "closed_loop_smoke" &&
      parsed.rateSemantics === "burst_cycle_rate" &&
      parsed.burstCycleRatePerSecond === parsed.metrics.l2Admission.perSecond &&
      parsed.finalityObserver.mode === "post-submit-bounded") ||
    (parsed.loadModel === "open-loop-upper-bound" &&
      parsed.openLoop !== undefined &&
      parsed.corpusShape !== undefined &&
      parsed.classification !== "closed_loop_smoke" &&
      parsed.rateSemantics === "offered_tps_uncalibrated" &&
      parsed.burstCycleRatePerSecond === null &&
      parsed.finalityObserver.mode === "aggregate-window");
  if (
    !statusReasonIsCanonical ||
    !loadModelLanguageIsCanonical ||
    !isDeepStrictEqual(parsed.measurementPolicy, expectedPolicy) ||
    expectedSubmissionDurationMs < 0 ||
    expectedDurationMs < expectedSubmissionDurationMs ||
    parsed.submissionDurationMs !== expectedSubmissionDurationMs ||
    parsed.durationMs !== expectedDurationMs ||
    expectedNotStartedCount < 0 ||
    parsed.notStartedCount !== expectedNotStartedCount ||
    parsed.submittedCount !== expectedSubmittedCount ||
    parsed.submissionFailedCount !== expectedSubmissionFailedCount ||
    parsed.acceptedCount !== expectedAcceptedCount ||
    parsed.acceptanceNotObservedCount !== expectedAcceptanceNotObservedCount ||
    parsed.acceptanceTimedOutCount !== expectedAcceptanceTimedOutCount ||
    parsed.finalityTimedOutCount !== expectedFinalityTimedOutCount ||
    parsed.observedCommittedCount !== expectedObservedCommittedCount ||
    parsed.unknownFinalityCount !== expectedUnknownFinalityCount ||
    parsed.rejectedCount !== expectedRejectedCount ||
    parsed.submittedCount + parsed.submissionFailedCount !==
      parsed.transactions.length ||
    new Set(transactionIndexes).size !== transactionIndexes.length ||
    transactionIndexes.some(
      (index, position) =>
        index < 0 ||
        index >= parsed.requestedCount ||
        (position > 0 && index <= transactionIndexes[position - 1]!),
    ) ||
    new Set(transactionHashes).size !== transactionHashes.length ||
    parsed.finalityObserver.maxObservedConcurrentRequests >
      parsed.finalityObserver.maxConcurrentRequests ||
    (parsed.finalityObserver.mode === "aggregate-window" &&
      (parsed.finalityObserver.maxConcurrentRequests !== 0 ||
        parsed.finalityObserver.maxObservedConcurrentRequests !== 0 ||
        parsed.finalityObserver.observedTransactionCount !==
          parsed.transactions.length ||
        parsed.finalityObserver.pollRequestCount !== 0)) ||
    (parsed.finalityObserver.mode === "post-submit-bounded" &&
      (parsed.finalityObserver.observedTransactionCount >
        parsed.acceptedCount ||
        parsed.finalityObserver.errorCount >
          parsed.finalityObserver.pollRequestCount ||
        parsed.finalityObserver.batchCount >
          parsed.finalityObserver.pollRequestCount))
  ) {
    throw new Error(
      `${label} status, policy, chronology, counts, identities, or observer evidence is inconsistent`,
    );
  }
  for (const [position, transaction] of parsed.transactions.entries()) {
    const transactionLabel = `${label}.transactions[${position.toString()}]`;
    const submitted = transaction.submission.status === "submitted";
    const submissionHasCanonicalFields =
      (submitted &&
        transaction.txHash !== null &&
        transaction.submission.submittedAt !== null &&
        transaction.submission.durationMs !== undefined &&
        transaction.submission.error === undefined &&
        transaction.selectedInputs.length > 0) ||
      (!submitted &&
        transaction.txHash === null &&
        transaction.submission.submittedAt === null &&
        transaction.submission.durationMs === undefined &&
        transaction.submission.error !== undefined &&
        transaction.selectedInputs.length === 0);
    const acceptanceHasCanonicalFields =
      (transaction.acceptance.status === "accepted" &&
        transaction.acceptance.error === undefined) ||
      (transaction.acceptance.status === "rejected" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined) ||
      (transaction.acceptance.status === "timeout" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined &&
        transaction.acceptance.error !== undefined) ||
      (transaction.acceptance.status === "not_observed" &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined) ||
      (transaction.acceptance.status === "not_submitted" &&
        !submitted &&
        transaction.acceptance.acceptedAt === undefined &&
        transaction.acceptance.durationMs === undefined &&
        transaction.acceptance.error !== undefined);
    const finalityHasCanonicalFields =
      (transaction.finality.status === "committed" &&
        transaction.acceptance.status === "accepted" &&
        transaction.finality.committedAt !== undefined &&
        transaction.finality.durationMs !== undefined &&
        transaction.finality.error === undefined) ||
      (transaction.finality.status === "rejected" &&
        (transaction.acceptance.status === "accepted" ||
          transaction.acceptance.status === "rejected") &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined &&
        transaction.finality.error !== undefined) ||
      (transaction.finality.status === "timeout" &&
        transaction.acceptance.status === "accepted" &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined &&
        transaction.finality.error !== undefined) ||
      (transaction.finality.status === "not_observed" &&
        transaction.finality.committedAt === undefined &&
        transaction.finality.durationMs === undefined);
    const submittedAtMs =
      transaction.submission.submittedAt === null
        ? null
        : Date.parse(transaction.submission.submittedAt);
    const acceptedAtMs =
      transaction.acceptance.acceptedAt === undefined
        ? null
        : Date.parse(transaction.acceptance.acceptedAt);
    const committedAtMs =
      transaction.finality.committedAt === undefined
        ? null
        : Date.parse(transaction.finality.committedAt);
    if (
      !submissionHasCanonicalFields ||
      !acceptanceHasCanonicalFields ||
      !finalityHasCanonicalFields ||
      (submitted && transaction.acceptance.status === "not_submitted") ||
      (!submitted && transaction.acceptance.status !== "not_submitted") ||
      transaction.workerIndex >= parsed.concurrency ||
      new Set(transaction.selectedInputs).size !==
        transaction.selectedInputs.length ||
      (submittedAtMs !== null &&
        (submittedAtMs < startedAtMs ||
          submittedAtMs > submissionFinishedAtMs)) ||
      (acceptedAtMs !== null &&
        (submittedAtMs === null ||
          acceptedAtMs < submittedAtMs ||
          acceptedAtMs > finishedAtMs ||
          (transaction.acceptance.durationMs !== undefined &&
            transaction.acceptance.durationMs !==
              acceptedAtMs - submittedAtMs))) ||
      (committedAtMs !== null &&
        (submittedAtMs === null ||
          committedAtMs < submittedAtMs ||
          committedAtMs > finishedAtMs ||
          (acceptedAtMs !== null && committedAtMs < acceptedAtMs) ||
          transaction.finality.durationMs !== committedAtMs - submittedAtMs))
    ) {
      throw new Error(
        `${transactionLabel} status, identity, or chronology is inconsistent`,
      );
    }
  }
  const clientSubmission = parsed.metrics.clientSubmission;
  const expectedClientDurationMs = submissionFinishedAtMs - startedAtMs;
  const expectedClientPerSecond =
    parsed.submittedCount === 0 || expectedClientDurationMs <= 0
      ? null
      : roundMetric(parsed.submittedCount / (expectedClientDurationMs / 1_000));
  if (
    clientSubmission.count !== parsed.submittedCount ||
    clientSubmission.missingCount !==
      Math.max(0, parsed.requestedCount - parsed.submittedCount) ||
    (parsed.submittedCount === 0
      ? clientSubmission.status !== "unavailable"
      : clientSubmission.status !==
        (parsed.submittedCount >= parsed.requestedCount
          ? "complete"
          : "partial")) ||
    (parsed.submittedCount === 0
      ? clientSubmission.startedAt !== null ||
        clientSubmission.finishedAt !== null ||
        clientSubmission.durationMs !== null ||
        clientSubmission.perSecond !== null
      : clientSubmission.startedAt !== parsed.startedAt ||
        clientSubmission.finishedAt !== parsed.submissionFinishedAt ||
        clientSubmission.durationMs !== expectedClientDurationMs ||
        clientSubmission.perSecond !== expectedClientPerSecond) ||
    clientSubmission.source !== "stress_artifact.submissions" ||
    clientSubmission.precision !== "artifact_timestamp" ||
    parsed.metrics.durableAdmission.count > parsed.submittedCount ||
    parsed.metrics.l2Admission.count > parsed.submittedCount ||
    parsed.metrics.l1Commit.l2Transactions.count > parsed.submittedCount ||
    parsed.metrics.immutableObservation.count > parsed.submittedCount ||
    parsed.metrics.fullFinality.count > parsed.submittedCount
  ) {
    throw new Error(`${label} metric windows contradict the run summary`);
  }
  if (parsed.openLoop !== undefined) {
    const { corpus, submission, placement } = parsed.openLoop;
    const corpusHashes = corpus.rows.map((row) => row.txHash);
    const corpusInputOutrefs = corpus.rows.map(
      (row) => row.selectedInputOutref,
    );
    if (
      parsed.corpusShape !== corpus.corpusShape ||
      parsed.requestedCount !== corpus.selectedTransactionCount ||
      corpus.rows.length !== corpus.selectedTransactionCount ||
      corpus.requiredTransactionCount < corpus.selectedTransactionCount ||
      corpus.rows.some(
        (row) =>
          row.corpusSliceId !== corpus.corpusSliceId ||
          (corpus.corpusShape !== "mixed" &&
            row.planShape !== corpus.corpusShape),
      ) ||
      new Set(corpusHashes).size !== corpusHashes.length ||
      new Set(corpusInputOutrefs).size !== corpusInputOutrefs.length ||
      transactionHashes.some((hash) => !corpusHashes.includes(hash)) ||
      parsed.transactions.length !== submission.offeredCount ||
      parsed.submittedCount !== submission.submittedCount ||
      parsed.submissionFailedCount !== submission.failedCount ||
      submission.offeredCount !==
        submission.submittedCount + submission.failedCount ||
      submission.maxObservedInFlight > submission.maxInFlight ||
      submission.maxInFlight !== parsed.openLoop.maxInFlight ||
      submission.targetRateTps !== parsed.openLoop.targetRateTps ||
      submission.durationMs !==
        Date.parse(submission.finishedAtIso) -
          Date.parse(submission.startedAtIso) ||
      submission.scheduleSlipMs.p50 > submission.scheduleSlipMs.p95 ||
      submission.scheduleSlipMs.p95 > submission.scheduleSlipMs.p99 ||
      submission.scheduleSlipMs.p99 > submission.scheduleSlipMs.max ||
      placement.validForUpperBoundClaim !==
        (!placement.insideMidgardNodeProcess &&
          !placement.insideMidgardNodeContainer)
    ) {
      throw new Error(`${label}.openLoop evidence is internally inconsistent`);
    }
  }
  return parsed;
}

const appendEvent = async (
  eventsNdjsonPath: string,
  event: Readonly<Record<string, unknown>>,
): Promise<void> => {
  await appendFile(eventsNdjsonPath, `${JSON.stringify(event)}\n`, "utf8");
};

async function* readNdjsonLines(path: string): AsyncGenerator<string> {
  const reader = readline.createInterface({
    input: createReadStream(path, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  for await (const line of reader) {
    const trimmed = line.trim();
    if (trimmed.length > 0) {
      yield trimmed;
    }
  }
}

const readJsonFile = async (path: string): Promise<unknown> => {
  const chunks: string[] = [];
  for await (const chunk of createReadStream(path, { encoding: "utf8" })) {
    chunks.push(chunk);
  }
  return JSON.parse(chunks.join("")) as unknown;
};

const readSubmitRecords = async (
  path: string,
): Promise<readonly OpenLoopSubmitRecord[]> => {
  const records: OpenLoopSubmitRecord[] = [];
  let index = 0;
  for await (const line of readNdjsonLines(path)) {
    index += 1;
    const parsed = JSON.parse(line) as Partial<OpenLoopSubmitRecord>;
    if (
      typeof parsed.txHash !== "string" ||
      typeof parsed.scheduledAtMs !== "number" ||
      typeof parsed.submittedAtMs !== "number" ||
      typeof parsed.scheduleSlipMs !== "number" ||
      typeof parsed.latencyMs !== "number" ||
      !("statusCode" in parsed) ||
      !("error" in parsed)
    ) {
      throw new Error(`submit-records row ${index.toString()} is invalid`);
    }
    records.push({
      txHash: parsed.txHash.toLowerCase(),
      scheduledAtMs: parsed.scheduledAtMs,
      submittedAtMs: parsed.submittedAtMs,
      scheduleSlipMs: parsed.scheduleSlipMs,
      latencyMs: parsed.latencyMs,
      statusCode:
        typeof parsed.statusCode === "number" ? parsed.statusCode : null,
      responseTxId:
        typeof parsed.responseTxId === "string" ? parsed.responseTxId : null,
      error: typeof parsed.error === "string" ? parsed.error : null,
    });
  }
  return records;
};

const readCorpusRowsForRecords = async ({
  corpusPath,
  records,
}: {
  readonly corpusPath: string;
  readonly records: readonly OpenLoopSubmitRecord[];
}): Promise<ReadonlyMap<string, OpenLoopCorpusRow>> => {
  const needed = new Set(records.map((record) => record.txHash.toLowerCase()));
  const found = new Map<string, OpenLoopCorpusRow>();
  if (needed.size === 0) {
    return found;
  }
  let index = 0;
  for await (const line of readNdjsonLines(corpusPath)) {
    index += 1;
    const row = parseOpenLoopCorpusLine(line, index);
    if (needed.has(row.txHash)) {
      found.set(row.txHash, row);
      if (found.size === needed.size) {
        break;
      }
    }
  }
  const missing = [...needed].filter((txHash) => !found.has(txHash));
  if (missing.length > 0) {
    throw new Error(
      `corpus lookup missed ${missing.length.toString()} submitted tx hashes`,
    );
  }
  return found;
};

const canonicalEnginePaths = (
  outDir: string,
): CanonicalEngineArtifactPaths => ({
  engineReportJson: join(outDir, "engine-report.json"),
  engineEventsNdjson: join(outDir, "engine-events.ndjson"),
  submitRecordsNdjson: join(outDir, "submit-records.ndjson"),
  noopCalibrationJson: join(outDir, "noop-calibration.json"),
  stdoutLog: join(outDir, "engine.stdout.log"),
  stderrLog: join(outDir, "engine.stderr.log"),
});

const runCanonicalEngineProcess = async ({
  config,
  paths,
  signal,
}: {
  readonly config: E2EL2StressConfig;
  readonly paths: CanonicalEngineArtifactPaths;
  readonly signal?: AbortSignal;
}): Promise<CanonicalEngineRunResult> =>
  await new Promise((resolve, reject) => {
    const child = spawn(
      process.execPath,
      ["scripts/throughput-valid-stress.mjs"],
      {
        cwd: process.cwd(),
        env: {
          ...process.env,
          STRESS_SUBMIT_ENDPOINT: config.nodeEndpoint,
          STRESS_MODE: "open",
          STRESS_OPEN_LOOP_RATE_TPS: config.targetRateTps.toString(),
          STRESS_MEASURED_SEC: (config.openLoopDurationMs / 1000).toString(),
          STRESS_SUBMIT_CONCURRENCY: config.openLoopMaxInFlight.toString(),
          STRESS_HTTP_CONNECTIONS: config.openLoopMaxInFlight.toString(),
          STRESS_CORPUS_PATH: config.corpusPath ?? "",
          STRESS_CORPUS_SHAPE: config.corpusShape,
          STRESS_CORPUS_SLICE_ID: config.corpusSliceId,
          STRESS_MAX_CHAINS: "auto",
          STRESS_REPORT_PATH: paths.engineReportJson,
          STRESS_ENGINE_EVENTS_PATH: paths.engineEventsNdjson,
          STRESS_SUBMIT_RECORDS_PATH: paths.submitRecordsNdjson,
          STRESS_NOOP_CALIBRATION_PATH: paths.noopCalibrationJson,
          ...(config.noOpCalibrationEndpoint === undefined
            ? {}
            : { STRESS_NOOP_ENDPOINT: config.noOpCalibrationEndpoint }),
          STRESS_REQUIRE_NOOP_CALIBRATION: config.requireNoOpCalibration
            ? "true"
            : "false",
          STRESS_CALIBRATION_DURATION_SEC: (
            config.noOpCalibrationDurationMs / 1000
          ).toString(),
          STRESS_CLIENT_SELF_CHECK: "false",
          STRESS_REQUIRE_METRIC_PRESENCE: "false",
        },
      },
    );
    const stdout = createWriteStream(paths.stdoutLog, { flags: "w" });
    const stderr = createWriteStream(paths.stderrLog, { flags: "w" });
    child.stdout.pipe(stdout);
    child.stderr.pipe(stderr);
    const abort = (): void => {
      child.kill("SIGTERM");
      setTimeout(() => child.kill("SIGKILL"), 5_000).unref();
    };
    signal?.addEventListener("abort", abort, { once: true });
    child.once("error", reject);
    child.once("close", (exitCode, exitSignal) => {
      signal?.removeEventListener("abort", abort);
      stdout.end();
      stderr.end();
      resolve({
        exitCode: exitCode ?? 1,
        signal: exitSignal,
      });
    });
  });

const formatMetricValue = (value: number | null): string =>
  value === null ? "-" : value.toString();

const formatMetricNotes = (notes: readonly string[]): string =>
  notes.length === 0 ? "-" : notes.join(",");

const renderStressSummaryMarkdown = (summary: E2EL2StressSummary): string => {
  const metricRows = flattenStressMetricRows(summary.metrics);
  const lines = [
    "# Midgard L2 Stress Summary",
    "",
    `- runId: ${summary.runId}`,
    `- status: ${summary.status}`,
    ...(summary.interruptedReason === undefined
      ? []
      : [`- interruptedReason: ${summary.interruptedReason}`]),
    `- loadModel: ${summary.loadModel}`,
    `- workloadProfile: ${summary.workloadProfile}`,
    `- classification: ${summary.classification}`,
    `- rateSemantics: ${summary.rateSemantics}`,
    ...(summary.rateSemantics !== "burst_cycle_rate"
      ? []
      : [
          `- burstCycleRatePerSecond: ${summary.burstCycleRatePerSecond?.toString() ?? "n/a"} (bounded by concurrency=${summary.concurrency.toString()}; NOT a production throughput measurement; see docs/exec-plans/phase-0-stop-the-bleeding.md)`,
        ]),
    `- mode: ${summary.mode}`,
    ...(summary.corpusShape === undefined
      ? []
      : [`- corpusShape: ${summary.corpusShape}`]),
    `- advanceOn: ${summary.measurementPolicy.advanceOn}`,
    `- primaryStageMetric: ${summary.measurementPolicy.primaryStageMetric}`,
    `- finalityObservation: ${summary.measurementPolicy.finalityObservation}`,
    `- fullFinalityRequiresDrainProof: ${summary.measurementPolicy.fullFinalityRequiresDrainProof.toString()}`,
    `- requestedCount: ${summary.requestedCount.toString()}`,
    `- notStartedCount: ${summary.notStartedCount.toString()}`,
    `- submittedCount: ${summary.submittedCount.toString()}`,
    `- submissionFailedCount: ${summary.submissionFailedCount.toString()}`,
    `- acceptedCount: ${summary.acceptedCount.toString()}`,
    `- acceptanceNotObservedCount: ${summary.acceptanceNotObservedCount.toString()}`,
    `- acceptanceTimedOutCount: ${summary.acceptanceTimedOutCount.toString()}`,
    `- finalityTimedOutCount: ${summary.finalityTimedOutCount.toString()}`,
    `- observedCommittedCount: ${summary.observedCommittedCount.toString()}`,
    `- unknownFinalityCount: ${summary.unknownFinalityCount.toString()}`,
    `- rejectedCount: ${summary.rejectedCount.toString()}`,
    `- concurrency: ${summary.concurrency.toString()}`,
    `- finalityObserverMaxConcurrentRequests: ${summary.finalityObserver.maxConcurrentRequests.toString()}`,
    `- finalityObserverMaxObservedConcurrentRequests: ${summary.finalityObserver.maxObservedConcurrentRequests.toString()}`,
    `- finalityObserverPollRequestCount: ${summary.finalityObserver.pollRequestCount.toString()}`,
    "",
    "## Stage Metrics",
    "",
    "| metric | status | count | missing | duration_s | rate_per_s* | precision | source | notes |",
    "| --- | --- | ---: | ---: | ---: | ---: | --- | --- | --- |",
    ...metricRows.map(([label, metric]) => {
      const durationSeconds =
        metric.durationMs === null ? null : metric.durationMs / 1000;
      return `| ${label} | ${metric.status} | ${metric.count.toString()} | ${metric.missingCount.toString()} | ${formatMetricValue(durationSeconds)} | ${formatMetricValue(metric.perSecond)} | ${metric.precision} | ${metric.source} | ${formatMetricNotes(metric.notes)} |`;
    }),
    "",
    "* rate_per_s is a raw count/duration ratio; read it together with rateSemantics above. It is not production throughput when rateSemantics=burst_cycle_rate.",
    "",
    "## Transactions",
    "",
    "| index | submission | acceptance | finality | txHash | sender | destination | selectedInputs |",
    "| ---: | --- | --- | --- | --- | --- | --- | --- |",
    ...summary.transactions.map(
      (tx) =>
        `| ${tx.index.toString()} | ${tx.submission.status} | ${tx.acceptance.status} | ${tx.finality.status} | ${tx.txHash ?? "-"} | ${tx.senderAddress} | ${tx.destinationAddress} | ${tx.selectedInputs.join(",") || "-"} |`,
    ),
    "",
  ];
  return lines.join("\n");
};

const readTxStatus = async ({
  fetchImpl,
  nodeEndpoint,
  signal,
  txHash,
}: {
  readonly fetchImpl: typeof fetch;
  readonly nodeEndpoint: string;
  readonly signal?: AbortSignal;
  readonly txHash: string;
}): Promise<{
  readonly statusCode: number;
  readonly body: unknown;
}> => {
  throwIfAborted(signal);
  const response = await fetchImpl(
    `${nodeEndpoint}/tx-status?tx_hash=${encodeURIComponent(txHash)}`,
    signal === undefined ? undefined : { signal },
  );
  const responseText = await response.text();
  let body: unknown = responseText;
  try {
    body = JSON.parse(responseText) as unknown;
  } catch {
    // Keep the raw response text for event evidence.
  }
  return {
    statusCode: response.status,
    body,
  };
};

const txStatusFromBody = (body: unknown): string | null => {
  if (typeof body !== "object" || body === null) {
    return null;
  }
  const status = (body as { readonly status?: unknown }).status;
  return typeof status === "string" ? status : null;
};

const isAcceptedOrLaterTxStatus = (status: string | null): boolean =>
  status === "accepted" ||
  status === "pending_commit" ||
  status === "awaiting_local_recovery" ||
  status === "committed";

const pollUntilAccepted = async ({
  config,
  eventsNdjsonPath,
  fetchImpl,
  sleepImpl,
  signal,
  now,
  txHash,
  submittedAtMs,
}: {
  readonly config: E2EL2StressConfig;
  readonly eventsNdjsonPath: string;
  readonly fetchImpl: typeof fetch;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly signal?: AbortSignal;
  readonly now: () => Date;
  readonly txHash: string;
  readonly submittedAtMs: number;
}): Promise<{
  readonly acceptance: E2EL2StressAcceptanceState;
  readonly finality: E2EL2StressFinalityState;
}> => {
  const deadlineMs = submittedAtMs + config.acceptanceTimeoutMs;
  let attempt = 0;
  while (true) {
    throwIfAborted(signal);
    const polledAt = now();
    try {
      const probe = await readTxStatus({
        fetchImpl,
        nodeEndpoint: config.nodeEndpoint,
        signal,
        txHash,
      });
      const observedStatus = txStatusFromBody(probe.body);
      await appendEvent(eventsNdjsonPath, {
        event: "tx_status",
        at: polledAt.toISOString(),
        txHash,
        statusCode: probe.statusCode,
        status: observedStatus,
      });
      if (isAcceptedOrLaterTxStatus(observedStatus)) {
        const elapsedMs = Math.max(0, polledAt.getTime() - submittedAtMs);
        return {
          acceptance: {
            status: "accepted",
            acceptedAt: polledAt.toISOString(),
            durationMs: elapsedMs,
          },
          ...(observedStatus === "committed"
            ? {
                finality: {
                  status: "committed",
                  committedAt: polledAt.toISOString(),
                  durationMs: elapsedMs,
                },
              }
            : { finality: { status: "not_observed" } }),
        };
      }
      if (observedStatus === "rejected") {
        const error = formatJson(probe.body);
        return {
          acceptance: {
            status: "rejected",
            error,
          },
          finality: {
            status: "rejected",
            error,
          },
        };
      }
    } catch (error) {
      await appendEvent(eventsNdjsonPath, {
        event: "tx_status_error",
        at: polledAt.toISOString(),
        txHash,
        error: errorMessage(error),
      });
    }

    if (now().getTime() >= deadlineMs) {
      return {
        acceptance: {
          status: "timeout",
          error: `Timed out waiting for /tx-status accepted after ${config.acceptanceTimeoutMs.toString()}ms.`,
        },
        finality: {
          status: "not_observed",
        },
      };
    }
    await sleepWithAbort(sleepImpl, nextPollDelayMs(attempt, config), signal);
    attempt += 1;
  }
};

type PendingFinalityTransaction = {
  readonly tx: E2EL2StressTransaction & {
    readonly txHash: string;
    readonly submission: E2EL2StressSubmissionState & {
      readonly submittedAt: string;
    };
  };
  readonly submittedAtMs: number;
};

type FinalityObserverResult = {
  readonly transactions: readonly E2EL2StressTransaction[];
  readonly summary: E2EL2StressFinalityObserverSummary;
  readonly interruptedReason?: string;
};

const observeFinalityBounded = async ({
  config,
  eventsNdjsonPath,
  fetchImpl,
  sleepImpl,
  signal,
  now,
  transactions,
}: {
  readonly config: E2EL2StressConfig;
  readonly eventsNdjsonPath: string;
  readonly fetchImpl: typeof fetch;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly signal?: AbortSignal;
  readonly now: () => Date;
  readonly transactions: readonly PendingFinalityTransaction[];
}): Promise<FinalityObserverResult> => {
  const maxConcurrentRequests = Math.max(
    1,
    config.finalityObserverMaxConcurrentRequests,
  );
  const observerStartedAt = now();
  const observerStartedAtMs = observerStartedAt.getTime();
  const pending = transactions.map((entry) => ({
    ...entry,
    deadlineMs: observerStartedAtMs + config.commitObservationTimeoutMs,
    nextPollAtMs: observerStartedAtMs,
    pollAttempt: 0,
  }));
  const completed: E2EL2StressTransaction[] = [];
  let maxObservedConcurrentRequests = 0;
  let activeRequests = 0;
  let pollRequestCount = 0;
  let batchCount = 0;
  let errorCount = 0;
  let interruptedReason: string | undefined;

  await appendEvent(eventsNdjsonPath, {
    event: "stress.observer.started",
    at: observerStartedAt.toISOString(),
    mode: "post-submit-bounded",
    transactionCount: pending.length,
    maxConcurrentRequests,
  });

  while (pending.length > 0) {
    if (signalWasAborted(signal)) {
      interruptedReason = abortReason(signal);
      break;
    }
    const cycleAt = now();
    const cycleAtMs = cycleAt.getTime();
    for (let index = pending.length - 1; index >= 0; index -= 1) {
      const entry = pending[index]!;
      if (cycleAtMs > entry.deadlineMs) {
        completed.push({
          ...entry.tx,
          finality: {
            status: "timeout",
            error: `Timed out waiting for /tx-status committed after ${config.commitObservationTimeoutMs.toString()}ms.`,
          },
        });
        pending.splice(index, 1);
      }
    }
    if (pending.length === 0) {
      break;
    }
    const due = pending
      .filter((entry) => entry.nextPollAtMs <= cycleAtMs)
      .slice(0, maxConcurrentRequests);
    if (due.length === 0) {
      const nextAtMs = Math.min(
        ...pending.map((entry) =>
          Math.min(entry.nextPollAtMs, entry.deadlineMs),
        ),
      );
      try {
        await sleepWithAbort(
          sleepImpl,
          Math.max(1, nextAtMs - cycleAtMs),
          signal,
        );
      } catch (error) {
        if (isAbortLikeError(error) && signalWasAborted(signal)) {
          interruptedReason = abortReason(signal);
          break;
        }
        throw error;
      }
      continue;
    }

    batchCount += 1;
    const outcomes = await Promise.all(
      due.map(async (entry) => {
        activeRequests += 1;
        maxObservedConcurrentRequests = Math.max(
          maxObservedConcurrentRequests,
          activeRequests,
        );
        try {
          pollRequestCount += 1;
          const probe = await readTxStatus({
            fetchImpl,
            nodeEndpoint: config.nodeEndpoint,
            signal,
            txHash: entry.tx.txHash,
          });
          return {
            entry,
            polledAt: now(),
            statusCode: probe.statusCode,
            status: txStatusFromBody(probe.body),
            body: probe.body,
          };
        } catch (error) {
          if (isAbortLikeError(error) && signalWasAborted(signal)) {
            return {
              entry,
              polledAt: now(),
              error: new StressInterruptedError(abortReason(signal)),
            };
          }
          return { entry, polledAt: now(), error };
        } finally {
          activeRequests -= 1;
        }
      }),
    );

    let committedCount = 0;
    let rejectedCount = 0;
    let pendingCount = 0;
    for (const outcome of outcomes) {
      const pendingIndex = pending.findIndex(
        (entry) => entry.tx.index === outcome.entry.tx.index,
      );
      if (pendingIndex < 0) {
        continue;
      }
      if ("error" in outcome) {
        if (
          outcome.error instanceof StressInterruptedError &&
          signalWasAborted(signal)
        ) {
          interruptedReason = outcome.error.message;
          break;
        }
        errorCount += 1;
        pending[pendingIndex] = {
          ...outcome.entry,
          nextPollAtMs:
            outcome.polledAt.getTime() +
            nextPollDelayMs(outcome.entry.pollAttempt, config),
          pollAttempt: outcome.entry.pollAttempt + 1,
        };
        continue;
      }
      if (outcome.status === "committed") {
        committedCount += 1;
        completed.push({
          ...outcome.entry.tx,
          finality: {
            status: "committed",
            committedAt: outcome.polledAt.toISOString(),
            durationMs: Math.max(
              0,
              outcome.polledAt.getTime() - outcome.entry.submittedAtMs,
            ),
          },
        });
        pending.splice(pendingIndex, 1);
        continue;
      }
      if (outcome.status === "rejected") {
        rejectedCount += 1;
        completed.push({
          ...outcome.entry.tx,
          finality: {
            status: "rejected",
            error: formatJson(outcome.body),
          },
        });
        pending.splice(pendingIndex, 1);
        continue;
      }
      pendingCount += 1;
      pending[pendingIndex] = {
        ...outcome.entry,
        nextPollAtMs:
          outcome.polledAt.getTime() +
          nextPollDelayMs(outcome.entry.pollAttempt, config),
        pollAttempt: outcome.entry.pollAttempt + 1,
      };
    }
    await appendEvent(eventsNdjsonPath, {
      event: "stress.observer.batch_polled",
      at: now().toISOString(),
      batchCount,
      polledCount: outcomes.length,
      committedCount,
      rejectedCount,
      pendingCount,
      remainingCount: pending.length,
      maxConcurrentRequests,
      maxObservedConcurrentRequests,
    });
    if (interruptedReason !== undefined) {
      break;
    }
  }

  const finalTransactions = [...completed, ...pending.map((entry) => entry.tx)];
  await appendEvent(eventsNdjsonPath, {
    event: "stress.observer.finished",
    at: now().toISOString(),
    observedTransactionCount: transactions.length,
    completedCount: completed.length,
    remainingCount: pending.length,
    pollRequestCount,
    batchCount,
    errorCount,
    maxConcurrentRequests,
    maxObservedConcurrentRequests,
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
  });

  return {
    transactions: finalTransactions,
    summary: {
      mode: "post-submit-bounded",
      maxConcurrentRequests,
      maxObservedConcurrentRequests,
      observedTransactionCount: transactions.length,
      pollRequestCount,
      batchCount,
      errorCount,
    },
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
  };
};

const walletForWorker = (
  config: E2EL2StressConfig,
  workerIndex: number,
): StressWallet =>
  config.mode === "parallel-fanout"
    ? config.stressWallets[workerIndex]!
    : requirePrimaryWallet(config);

const requirePrimaryWallet = (config: E2EL2StressConfig): StressWallet => {
  if (config.primaryWallet === undefined) {
    throw new Error("serial-chain stress requires a primary wallet.");
  }
  return config.primaryWallet;
};

const spendableUtxosForLovelace = (
  utxos: readonly NodeUtxo[],
  lovelace: bigint,
): readonly NodeUtxo[] =>
  utxos.filter((utxo) => (utxo.assets.lovelace ?? 0n) >= lovelace);

type AggregateObserverRunSummary = {
  readonly sampleCount: number;
  readonly errorCount: number;
  readonly overloaded: boolean;
};

const runAggregateObserverDuring = async <A>({
  config,
  runtime,
  action,
  eventsNdjsonPath,
  sleepImpl,
  now,
}: {
  readonly config: E2EL2StressConfig;
  readonly runtime: E2EL2StressRuntime;
  readonly action: () => Promise<A>;
  readonly eventsNdjsonPath: string;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly now: () => Date;
}): Promise<{
  readonly result: A;
  readonly observer: AggregateObserverRunSummary;
}> => {
  if (runtime.collectAggregateObserverSample === undefined) {
    return {
      result: await action(),
      observer: { sampleCount: 0, errorCount: 0, overloaded: false },
    };
  }
  let done = false;
  let sampleCount = 0;
  let errorCount = 0;
  let overloaded = false;
  const observer = (async () => {
    while (!done) {
      const sampleStartedAt = now();
      const sampleStartedMs = sampleStartedAt.getTime();
      try {
        const sample = await runtime.collectAggregateObserverSample!({
          at: sampleStartedAt.toISOString(),
          runId: config.runId,
          loadModel: config.loadModel,
        });
        sampleCount += 1;
        const durationMs = Math.max(0, now().getTime() - sampleStartedMs);
        if (durationMs > config.aggregateObserverIntervalMs) {
          overloaded = true;
        }
        await appendEvent(eventsNdjsonPath, {
          event: "stress.aggregate_observer.sample",
          at: sampleStartedAt.toISOString(),
          durationMs,
          overloaded: durationMs > config.aggregateObserverIntervalMs,
          sample,
        });
      } catch (error) {
        errorCount += 1;
        await appendEvent(eventsNdjsonPath, {
          event: "stress.aggregate_observer.error",
          at: now().toISOString(),
          error: errorMessage(error),
        });
      }
      await sleepImpl(config.aggregateObserverIntervalMs);
    }
  })();
  try {
    const result = await action();
    done = true;
    await observer;
    return {
      result,
      observer: { sampleCount, errorCount, overloaded },
    };
  } catch (error) {
    done = true;
    await observer;
    throw error;
  }
};

const classifyOpenLoopRun = ({
  metrics,
  submission,
  calibration,
  placement,
  observer,
}: {
  readonly metrics: StressMetrics;
  readonly submission: OpenLoopSubmitSummary;
  readonly calibration?: NoOpCalibrationSummary;
  readonly placement: OpenLoopPlacementProof;
  readonly observer: AggregateObserverRunSummary;
}): E2EL2StressClassification => {
  if (
    !placement.validForUpperBoundClaim ||
    calibration?.passed === false ||
    submission.submittedOfferedRatio < 0.98
  ) {
    return "client_overloaded";
  }
  if (observer.overloaded || observer.errorCount > 0) {
    return "observer_overloaded";
  }
  if (
    metrics.durableAdmission.status !== "complete" ||
    metrics.durableAdmission.count < submission.submittedCount
  ) {
    return "admission_bottleneck";
  }
  if (
    metrics.l2Admission.status !== "complete" ||
    metrics.l2Admission.count < submission.submittedCount
  ) {
    return "validation_bottleneck";
  }
  if (metrics.fullFinality.status === "complete") {
    return "full_pipeline_sustained";
  }
  return "ingress_ok_commit_failed";
};

const runOpenLoopUpperBoundStress = async (
  config: E2EL2StressConfig,
  runtime: E2EL2StressRuntime,
): Promise<E2EL2StressRunResult> => {
  if (config.corpusPath === undefined) {
    throw new Error("open-loop upper-bound stress requires a corpusPath");
  }
  const sleepImpl = runtime.sleep ?? sleep;
  const now = runtime.now ?? (() => new Date());
  const signal = runtime.abortSignal;

  await mkdir(config.outDir, { recursive: true });
  const configJsonPath = join(config.outDir, "config.json");
  const eventsNdjsonPath = join(config.outDir, "events.ndjson");
  const summaryJsonPath = join(config.outDir, "summary.json");
  const summaryMarkdownPath = join(config.outDir, "summary.md");
  await writeFile(configJsonPath, `${formatJson(artifactConfig(config))}\n`, {
    encoding: "utf8",
    flag: "w",
  });

  const placement = buildOpenLoopPlacementProof();
  const startedAtDate = now();
  const startedAt = startedAtDate.toISOString();
  const enginePaths = canonicalEnginePaths(config.outDir);
  await appendEvent(eventsNdjsonPath, {
    event: "stress_started",
    at: startedAt,
    runId: config.runId,
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
    placement,
  });

  const { result: engineResult, observer } = await runAggregateObserverDuring({
    config,
    runtime,
    eventsNdjsonPath,
    sleepImpl,
    now,
    action: () =>
      (runtime.runCanonicalEngine ?? runCanonicalEngineProcess)({
        config,
        paths: enginePaths,
        signal,
      }),
  });

  const submissionFinishedAtDate = now();
  const submissionFinishedAt = submissionFinishedAtDate.toISOString();

  const engineReport = (await readJsonFile(enginePaths.engineReportJson)) as {
    readonly calibration?: { readonly noOp?: NoOpCalibrationSummary | null };
    readonly summary?: { readonly firstErrors?: readonly string[] };
  };
  const submitRecords = await readSubmitRecords(
    enginePaths.submitRecordsNdjson,
  );
  const corpusRowsByTxHash = await readCorpusRowsForRecords({
    corpusPath: config.corpusPath,
    records: submitRecords,
  });
  const corpusRows = submitRecords.flatMap((record) => {
    const row = corpusRowsByTxHash.get(record.txHash);
    return row === undefined ? [] : [row];
  });
  const corpus: OpenLoopCorpusPlan = {
    rows: corpusRows,
    requiredTransactionCount: submitRecords.length,
    selectedTransactionCount: submitRecords.length,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
  };
  const summaryStartedAtMs =
    submitRecords.length === 0
      ? startedAtDate.getTime()
      : Math.min(...submitRecords.map((record) => record.scheduledAtMs));
  const summaryFinishedAtMs =
    submitRecords.length === 0
      ? submissionFinishedAtDate.getTime()
      : Math.max(
          ...submitRecords.map(
            (record) => record.submittedAtMs + record.latencyMs,
          ),
        );
  const submitResult = {
    records: submitRecords,
    summary: summarizeOpenLoopSubmissions({
      records: submitRecords,
      offeredCount: submitRecords.length,
      targetRateTps: config.targetRateTps,
      maxInFlight: config.openLoopMaxInFlight,
      maxObservedInFlight: config.openLoopMaxInFlight,
      startedAtMs: summaryStartedAtMs,
      finishedAtMs: summaryFinishedAtMs,
      startedAtIso: new Date(summaryStartedAtMs).toISOString(),
      finishedAtIso: new Date(summaryFinishedAtMs).toISOString(),
    }),
  };
  const calibration = engineReport.calibration?.noOp ?? undefined;
  if (calibration !== undefined) {
    await appendEvent(eventsNdjsonPath, {
      event: "stress.no_op_calibration.finished",
      at: now().toISOString(),
      calibration,
    });
  }
  if (engineResult.exitCode !== 0 && submitRecords.length === 0) {
    throw new Error(
      `canonical stress engine exited with ${engineResult.exitCode.toString()} before submitting records`,
    );
  }
  let translatedStageIndex = 0;
  for await (const line of readNdjsonLines(enginePaths.engineEventsNdjson)) {
    const event = JSON.parse(line) as {
      readonly event?: string;
      readonly at?: string;
      readonly [key: string]: unknown;
    };
    if (event.event === "stage_started") {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.observer.started",
        at: event.at ?? now().toISOString(),
        stageName: event.name,
        targetRateTps: event.targetRateTps,
      });
    } else if (event.event === "counter_sample") {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.aggregate_observer.sample",
        at: event.at ?? now().toISOString(),
        source: "canonical_engine",
        counters: event.counters,
        phase: event.phase,
      });
    } else if (event.event === "stage_finished") {
      translatedStageIndex += 1;
      await appendEvent(eventsNdjsonPath, {
        event:
          translatedStageIndex === 1
            ? "stress_submission_finished"
            : "stress.observer.finished",
        at: event.at ?? now().toISOString(),
        stageName: event.name,
        submitted: event.submitted,
        submitErrors: event.submitErrors,
        abortedCorpusExhausted: event.aborted_corpus_exhausted,
      });
    }
  }
  const submittedTxHashes = submitResult.records.flatMap((record) =>
    record.statusCode !== null &&
    record.statusCode >= 200 &&
    record.statusCode < 300
      ? [record.txHash]
      : [],
  );
  let dbMetricSources: StressStageMetricDbSources | undefined;
  if (runtime.collectStageMetricSources !== undefined) {
    dbMetricSources = await runtime.collectStageMetricSources({
      txHashes: submittedTxHashes,
    });
    await appendEvent(eventsNdjsonPath, {
      event: "stress.stage_metrics.db_sources_collected",
      at: now().toISOString(),
      txHashCount: submittedTxHashes.length,
      l2AdmissionRows: dbMetricSources.l2Admissions.length,
      l1CommitRows: dbMetricSources.l1Commits.length,
      immutableRows: dbMetricSources.immutableObservations.length,
      residueRows: dbMetricSources.residue.length,
    });
  }
  const admissionByTxHash = new Map(
    (dbMetricSources?.l2Admissions ?? []).map((row) => [row.txHash, row]),
  );
  const corpusByTxHash = new Map(corpus.rows.map((row) => [row.txHash, row]));
  const transactions: E2EL2StressTransaction[] = submitResult.records
    .map((record, index): E2EL2StressTransaction => {
      const corpusRow = corpusByTxHash.get(record.txHash)!;
      const successfulSubmit =
        record.statusCode !== null &&
        record.statusCode >= 200 &&
        record.statusCode < 300 &&
        record.error === null;
      const admission = admissionByTxHash.get(record.txHash);
      const acceptedAt =
        admission?.status === "accepted" ? admission.terminalAt : null;
      return {
        index,
        phase: "stress",
        txHash: successfulSubmit ? record.txHash : null,
        senderAddress: corpusRow.senderWalletId,
        destinationAddress: corpusRow.outputOutrefs.join(","),
        selectedInputs: [corpusRow.selectedInputOutref],
        submission: successfulSubmit
          ? {
              status: "submitted",
              submittedAt: new Date(record.submittedAtMs).toISOString(),
              durationMs: record.latencyMs,
            }
          : {
              status: "failed",
              submittedAt: null,
              error:
                record.error ??
                `POST /submit returned ${record.statusCode?.toString() ?? "no_status"}`,
            },
        acceptance:
          admission?.status === "accepted"
            ? {
                status: "accepted",
                ...(acceptedAt === null ? {} : { acceptedAt }),
              }
            : admission?.status === "rejected"
              ? { status: "rejected" }
              : successfulSubmit
                ? { status: "not_observed" }
                : { status: "not_submitted" },
        finality: { status: "not_observed" },
        workerIndex: 0,
        walletSeedSource: corpusRow.senderWalletId,
      };
    })
    .sort((left, right) => left.index - right.index);
  const finishedAtDate = now();
  const finishedAt = finishedAtDate.toISOString();
  const submittedCount = transactions.filter(
    (tx) => tx.submission.status === "submitted",
  ).length;
  const acceptedCount = transactions.filter(
    (tx) => tx.acceptance.status === "accepted",
  ).length;
  const submissionFailedCount = transactions.filter(
    (tx) => tx.submission.status === "failed",
  ).length;
  const rejectedCount = transactions.filter(
    (tx) => tx.acceptance.status === "rejected",
  ).length;
  const metrics = buildStressMetrics({
    requestedCount: corpus.selectedTransactionCount,
    submittedCount,
    acceptedCount,
    observedCommittedCount: 0,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    transactions,
    ...(dbMetricSources === undefined ? {} : { dbSources: dbMetricSources }),
    ...(runtime.fullFinalityDrainProof === undefined
      ? {}
      : { fullFinalityDrainProof: runtime.fullFinalityDrainProof }),
  });
  const baseClassification = classifyOpenLoopRun({
    metrics,
    submission: submitResult.summary,
    calibration,
    placement,
    observer,
  });
  const groundTruth =
    runtime.collectGroundTruthMetrics === undefined
      ? undefined
      : await runtime.collectGroundTruthMetrics({
          windowStart: startedAt,
          windowEnd: finishedAt,
          txHashSample: submittedTxHashes.slice(0, 1_000),
          offeredCount: corpus.selectedTransactionCount,
          calibrationProofRef: enginePaths.noopCalibrationJson,
        });
  const fingerprint =
    groundTruth?.fingerprint ??
    (runtime.collectEnvironmentFingerprint === undefined
      ? undefined
      : await runtime.collectEnvironmentFingerprint({
          calibrationProofRef: enginePaths.noopCalibrationJson,
        }));
  const classification: E2EL2StressClassification = submitRecords.some(
    (record) => record.error?.includes("duplicate_or_mismatched_response"),
  )
    ? "duplicate_submission"
    : engineResult.exitCode !== 0 &&
        JSON.stringify(engineReport).includes("corpus_exhausted")
      ? "corpus_exhausted"
      : baseClassification;
  const summary = parseE2EL2StressSummaryV1({
    schemaVersion: E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    runId: config.runId,
    status: signalWasAborted(signal) ? "interrupted" : "completed",
    ...(signalWasAborted(signal)
      ? { interruptedReason: abortReason(signal) }
      : {}),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    classification,
    rateSemantics: "offered_tps_uncalibrated",
    burstCycleRatePerSecond: null,
    mode: config.mode,
    measurementPolicy: measurementPolicyForConfig(config),
    openLoop: {
      targetRateTps: config.targetRateTps,
      durationMs: config.openLoopDurationMs,
      maxInFlight: config.openLoopMaxInFlight,
      corpus,
      submission: submitResult.summary,
      ...(calibration === undefined ? {} : { calibration }),
      placement,
    },
    requestedCount: corpus.selectedTransactionCount,
    notStartedCount: Math.max(
      0,
      corpus.selectedTransactionCount - transactions.length,
    ),
    submittedCount,
    submissionFailedCount,
    acceptedCount,
    acceptanceNotObservedCount: transactions.filter(
      (tx) => tx.acceptance.status === "not_observed",
    ).length,
    acceptanceTimedOutCount: 0,
    finalityTimedOutCount: 0,
    observedCommittedCount: 0,
    unknownFinalityCount: acceptedCount,
    rejectedCount,
    concurrency: config.openLoopMaxInFlight,
    finalityObserver: {
      mode: "aggregate-window",
      maxConcurrentRequests: 0,
      maxObservedConcurrentRequests: 0,
      observedTransactionCount: transactions.length,
      pollRequestCount: 0,
      batchCount: observer.sampleCount,
      errorCount: observer.errorCount,
    },
    startedAt,
    submissionFinishedAt,
    finishedAt,
    submissionDurationMs: Math.max(
      0,
      submissionFinishedAtDate.getTime() - startedAtDate.getTime(),
    ),
    durationMs: Math.max(0, finishedAtDate.getTime() - startedAtDate.getTime()),
    metrics,
    ...(groundTruth === undefined ? {} : { groundTruth }),
    ...(fingerprint === undefined ? {} : { fingerprint }),
    latencyMs: {
      submitP50: submitResult.summary.scheduleSlipMs.p50,
      submitP95: submitResult.summary.scheduleSlipMs.p95,
      acceptanceP50: 0,
      acceptanceP95: 0,
      commitP50: 0,
      commitP95: 0,
    },
    artifactPaths: {
      configJson: configJsonPath,
      eventsNdjson: eventsNdjsonPath,
      summaryJson: summaryJsonPath,
      summaryMarkdown: summaryMarkdownPath,
      engineReportJson: enginePaths.engineReportJson,
      engineEventsNdjson: enginePaths.engineEventsNdjson,
      submitRecordsNdjson: enginePaths.submitRecordsNdjson,
      noOpCalibrationJson: enginePaths.noopCalibrationJson,
    },
    transactions,
  });
  await writeFile(summaryJsonPath, `${formatJson(summary)}\n`, "utf8");
  await writeFile(summaryMarkdownPath, renderStressSummaryMarkdown(summary), {
    encoding: "utf8",
  });
  await appendEvent(eventsNdjsonPath, {
    event: "stress_finished",
    at: finishedAt,
    summaryJsonPath,
    summaryMarkdownPath,
    submittedCount,
    acceptedCount,
    rejectedCount,
    submissionFailedCount,
    classification,
  });
  return {
    summary,
    configJsonPath,
    eventsNdjsonPath,
    summaryJsonPath,
    summaryMarkdownPath,
  };
};

export const runE2EL2StressThroughput = async (
  config: E2EL2StressConfig,
  runtime: E2EL2StressRuntime,
): Promise<E2EL2StressRunResult> => {
  if (config.loadModel === "open-loop-upper-bound") {
    return await runOpenLoopUpperBoundStress(config, runtime);
  }
  const fetchUtxos = runtime.fetchUtxos ?? fetchNodeUtxosByAddress;
  const fetchImpl = runtime.fetch ?? fetch;
  const sleepImpl = runtime.sleep ?? sleep;
  const now = runtime.now ?? (() => new Date());
  const signal = runtime.abortSignal;

  await mkdir(config.outDir, { recursive: true });
  const configJsonPath = join(config.outDir, "config.json");
  const eventsNdjsonPath = join(config.outDir, "events.ndjson");
  const summaryJsonPath = join(config.outDir, "summary.json");
  const summaryMarkdownPath = join(config.outDir, "summary.md");
  await writeFile(configJsonPath, `${formatJson(artifactConfig(config))}\n`, {
    encoding: "utf8",
    flag: "w",
  });

  const startedAtDate = now();
  const startedAt = startedAtDate.toISOString();
  await appendEvent(eventsNdjsonPath, {
    event: "stress_started",
    at: startedAt,
    runId: config.runId,
    configJsonPath,
  });

  if (config.mode === "parallel-fanout") {
    const requiredLovelace = config.lovelace + config.feeHeadroomLovelace;
    await Promise.all(
      config.stressWallets.slice(0, config.concurrency).map(async (wallet) => {
        const utxos = await fetchUtxos(config.nodeEndpoint, wallet.address);
        await appendEvent(eventsNdjsonPath, {
          event: "stress_wallet_preflight",
          at: now().toISOString(),
          address: wallet.address,
          seedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
          utxoCount: utxos.length,
          spendableUtxoCount: spendableUtxosForLovelace(utxos, requiredLovelace)
            .length,
          requiredLovelace: requiredLovelace.toString(10),
          transferLovelace: config.lovelace.toString(10),
          feeHeadroomLovelace: config.feeHeadroomLovelace.toString(10),
        });
        if (spendableUtxosForLovelace(utxos, requiredLovelace).length === 0) {
          throw new Error(
            `Stress wallet ${wallet.resolvedWalletSeedPhrase.resolvedFrom} has no spendable L2 UTxO with at least ${requiredLovelace.toString(10)} lovelace (transfer ${config.lovelace.toString(10)} + fee headroom ${config.feeHeadroomLovelace.toString(10)}) at ${wallet.address}. Fund independent stress wallets before rerunning parallel stress.`,
          );
        }
      }),
    );
  }

  const terminalTransactions: E2EL2StressTransaction[] = [];
  const pendingFinalityTransactions: PendingFinalityTransaction[] = [];
  const submitLatencies: number[] = [];
  const acceptanceLatencies: number[] = [];
  const commitLatencies: number[] = [];
  let interruptedReason: string | undefined;
  let submissionFailureCount = 0;

  const executeTransfer = async (
    index: number,
    workerIndex: number,
  ): Promise<void> => {
    if (signalWasAborted(signal)) {
      interruptedReason = interruptedReason ?? abortReason(signal);
      return;
    }
    const wallet = walletForWorker(config, workerIndex);
    const destinationAddress = config.destinationAddress ?? wallet.address;
    const submitStartedAt = now();
    await appendEvent(eventsNdjsonPath, {
      event: "transfer_submit_started",
      at: submitStartedAt.toISOString(),
      index,
      workerIndex,
      senderAddress: wallet.address,
      destinationAddress,
      walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
    });

    let submitResult: SubmitL2TransferResult | undefined;
    let submittedAtDate: Date | undefined;
    let submittedAt: string | undefined;
    let submitDurationMs: number | undefined;
    try {
      const transferConfig = parseSubmitL2TransferConfig({
        l2Address: destinationAddress,
        lovelace: config.lovelace.toString(10),
        assetSpecs: [],
        nodeEndpoint: config.nodeEndpoint,
        submitRequestTimeoutMs: config.submitRequestTimeoutMs,
        submissionMode: "api",
      });
      submitResult = await runtime.submitTransfer({
        index,
        phase: "stress",
        config: transferConfig,
        resolvedWalletSeedPhrase: wallet.resolvedWalletSeedPhrase,
        walletAddress: wallet.address,
        destinationAddress,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      });
      throwIfAborted(signal);
      submittedAtDate = now();
      submittedAt = submittedAtDate.toISOString();
      submitDurationMs = Math.max(
        0,
        submittedAtDate.getTime() - submitStartedAt.getTime(),
      );
      submitLatencies.push(submitDurationMs);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_submitted",
        at: submittedAt,
        index,
        workerIndex,
        txHash: submitResult.txId,
        submitStatus: submitResult.status,
        selectedInputs: submitResult.selectedInputs,
      });

      const accepted = await pollUntilAccepted({
        config,
        eventsNdjsonPath,
        fetchImpl,
        sleepImpl,
        signal,
        now,
        txHash: submitResult.txId,
        submittedAtMs: submittedAtDate.getTime(),
      });
      if (accepted.acceptance.durationMs !== undefined) {
        acceptanceLatencies.push(accepted.acceptance.durationMs);
      }
      const acceptedTx: E2EL2StressTransaction = {
        index,
        phase: "stress",
        txHash: submitResult.txId,
        senderAddress: submitResult.senderAddress,
        destinationAddress: submitResult.destinationAddress,
        selectedInputs: submitResult.selectedInputs,
        submission: {
          status: "submitted",
          submittedAt,
          durationMs: submitDurationMs,
        },
        acceptance: accepted.acceptance,
        finality: accepted.finality,
        workerIndex,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      };

      if (accepted.acceptance.status === "accepted") {
        await appendEvent(eventsNdjsonPath, {
          event: "transfer_accepted",
          at: accepted.acceptance.acceptedAt ?? now().toISOString(),
          index,
          workerIndex,
          txHash: submitResult.txId,
          acceptanceStatus: accepted.acceptance.status,
          finalityStatus: accepted.finality.status,
        });
      }

      if (accepted.finality.durationMs !== undefined) {
        commitLatencies.push(accepted.finality.durationMs);
      }

      if (
        accepted.acceptance.status === "accepted" &&
        accepted.finality.status === "not_observed"
      ) {
        pendingFinalityTransactions.push({
          tx: {
            ...acceptedTx,
            txHash: submitResult.txId,
            submission: {
              ...acceptedTx.submission,
              submittedAt,
            },
          },
          submittedAtMs: submittedAtDate.getTime(),
        });
        return;
      }

      terminalTransactions.push(acceptedTx);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_finished",
        at: now().toISOString(),
        index,
        workerIndex,
        txHash: submitResult.txId,
        acceptanceStatus: acceptedTx.acceptance.status,
        finalityStatus: acceptedTx.finality.status,
      });
    } catch (error) {
      if (isAbortLikeError(error) || signalWasAborted(signal)) {
        interruptedReason =
          interruptedReason ??
          (signalWasAborted(signal)
            ? abortReason(signal)
            : errorMessage(error));
        if (
          submitResult !== undefined &&
          submittedAtDate !== undefined &&
          submittedAt !== undefined &&
          submitDurationMs !== undefined
        ) {
          terminalTransactions.push({
            index,
            phase: "stress",
            txHash: submitResult.txId,
            senderAddress: submitResult.senderAddress,
            destinationAddress: submitResult.destinationAddress,
            selectedInputs: submitResult.selectedInputs,
            submission: {
              status: "submitted",
              submittedAt,
              durationMs: submitDurationMs,
            },
            acceptance: {
              status: "not_observed",
              error: interruptedReason,
            },
            finality: {
              status: "not_observed",
              error: interruptedReason,
            },
            workerIndex,
            walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
          });
        }
        await appendEvent(eventsNdjsonPath, {
          event: "stress.interrupt.received",
          at: now().toISOString(),
          index,
          workerIndex,
          reason: interruptedReason,
        });
        return;
      }
      const failedAt = now().toISOString();
      const message = errorMessage(error);
      const tx: E2EL2StressTransaction = {
        index,
        phase: "stress",
        txHash: null,
        senderAddress: wallet.address,
        destinationAddress,
        selectedInputs: [],
        submission: {
          status: "failed",
          submittedAt: null,
          error: message,
        },
        acceptance: {
          status: "not_submitted",
          error: message,
        },
        finality: {
          status: "not_observed",
        },
        workerIndex,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      };
      terminalTransactions.push(tx);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_failed",
        at: failedAt,
        index,
        workerIndex,
        error: message,
      });
      submissionFailureCount += 1;
      if (
        interruptedReason === undefined &&
        submissionFailureCount > config.maxSubmissionFailures
      ) {
        interruptedReason =
          `submission/build failure threshold exceeded: ${submissionFailureCount.toString()} failure(s) > ` +
          `allowed ${config.maxSubmissionFailures.toString()} (--max-submission-failures); aborting to avoid ` +
          `silently shrinking the sample. Last error: ${message}`;
        await appendEvent(eventsNdjsonPath, {
          event: "stress.abort.max_submission_failures_exceeded",
          at: now().toISOString(),
          submissionFailureCount,
          maxSubmissionFailures: config.maxSubmissionFailures,
          reason: interruptedReason,
        });
      }
    }
  };

  let nextIndex = 0;
  const workers = Array.from(
    { length: config.concurrency },
    async (_unused, workerIndex) => {
      while (true) {
        if (signalWasAborted(signal) || interruptedReason !== undefined) {
          interruptedReason =
            interruptedReason ??
            (signalWasAborted(signal) ? abortReason(signal) : "interrupted");
          return;
        }
        const index = nextIndex;
        nextIndex += 1;
        if (index >= config.count) {
          return;
        }
        await executeTransfer(index, workerIndex);
      }
    },
  );
  await Promise.all(workers);

  const submissionFinishedAtDate = now();
  const submissionFinishedAt = submissionFinishedAtDate.toISOString();
  await appendEvent(eventsNdjsonPath, {
    event: "stress_submission_finished",
    at: submissionFinishedAt,
    submittedCount:
      terminalTransactions.filter((tx) => tx.txHash !== null).length +
      pendingFinalityTransactions.length,
    acceptedCount:
      terminalTransactions.filter((tx) => tx.acceptance.status === "accepted")
        .length + pendingFinalityTransactions.length,
  });

  const finalityObserverResult: FinalityObserverResult =
    pendingFinalityTransactions.length === 0 || interruptedReason !== undefined
      ? {
          transactions: pendingFinalityTransactions.map((entry) => entry.tx),
          summary: {
            mode: "post-submit-bounded" as const,
            maxConcurrentRequests: Math.max(
              1,
              config.finalityObserverMaxConcurrentRequests,
            ),
            maxObservedConcurrentRequests: 0,
            observedTransactionCount: 0,
            pollRequestCount: 0,
            batchCount: 0,
            errorCount: 0,
          },
        }
      : await observeFinalityBounded({
          config,
          eventsNdjsonPath,
          fetchImpl,
          sleepImpl,
          signal,
          now,
          transactions: pendingFinalityTransactions,
        });
  interruptedReason =
    interruptedReason ?? finalityObserverResult.interruptedReason;
  for (const tx of finalityObserverResult.transactions) {
    if (tx.finality.durationMs !== undefined) {
      commitLatencies.push(tx.finality.durationMs);
    }
    await appendEvent(eventsNdjsonPath, {
      event: "transfer_finished",
      at: now().toISOString(),
      index: tx.index,
      workerIndex: tx.workerIndex,
      txHash: tx.txHash,
      acceptanceStatus: tx.acceptance.status,
      finalityStatus: tx.finality.status,
    });
  }
  const finishedAtDate = now();
  const finishedAt = finishedAtDate.toISOString();
  const sortedTransactions = [
    ...terminalTransactions,
    ...finalityObserverResult.transactions,
  ].sort((left, right) => left.index - right.index);
  const notStartedCount = Math.max(0, config.count - sortedTransactions.length);
  const submittedCount = sortedTransactions.filter(
    (tx) => tx.txHash !== null,
  ).length;
  const acceptedCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "accepted",
  ).length;
  const submissionFailedCount = sortedTransactions.filter(
    (tx) => tx.submission.status === "failed",
  ).length;
  const acceptanceNotObservedCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "not_observed",
  ).length;
  const acceptanceTimedOutCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "timeout",
  ).length;
  const finalityTimedOutCount = sortedTransactions.filter(
    (tx) => tx.finality.status === "timeout",
  ).length;
  const observedCommittedCount = sortedTransactions.filter(
    (tx) => tx.finality.status === "committed",
  ).length;
  const unknownFinalityCount = sortedTransactions.filter(
    (tx) =>
      tx.acceptance.status === "accepted" &&
      tx.finality.status === "not_observed",
  ).length;
  const rejectedCount = sortedTransactions.filter(
    (tx) =>
      tx.acceptance.status === "rejected" || tx.finality.status === "rejected",
  ).length;
  const durationMs = Math.max(
    0,
    finishedAtDate.getTime() - startedAtDate.getTime(),
  );
  const submissionDurationMs = Math.max(
    0,
    submissionFinishedAtDate.getTime() - startedAtDate.getTime(),
  );
  const stressTxHashes = sortedTransactions.flatMap((tx) =>
    tx.txHash === null ? [] : [tx.txHash],
  );
  let dbMetricSources: StressStageMetricDbSources | undefined;
  if (runtime.collectStageMetricSources !== undefined) {
    try {
      dbMetricSources = await runtime.collectStageMetricSources({
        txHashes: stressTxHashes,
      });
      await appendEvent(eventsNdjsonPath, {
        event: "stress.stage_metrics.db_sources_collected",
        at: now().toISOString(),
        txHashCount: stressTxHashes.length,
        l2AdmissionRows: dbMetricSources.l2Admissions.length,
        l1CommitRows: dbMetricSources.l1Commits.length,
        immutableRows: dbMetricSources.immutableObservations.length,
        residueRows: dbMetricSources.residue.length,
      });
    } catch (error) {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.stage_metrics.db_sources_failed",
        at: now().toISOString(),
        error: errorMessage(error),
      });
    }
  }
  const metrics = buildStressMetrics({
    requestedCount: config.count,
    submittedCount,
    acceptedCount,
    observedCommittedCount,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    transactions: sortedTransactions,
    ...(dbMetricSources === undefined ? {} : { dbSources: dbMetricSources }),
    ...(runtime.fullFinalityDrainProof === undefined
      ? {}
      : { fullFinalityDrainProof: runtime.fullFinalityDrainProof }),
  });
  const groundTruth =
    runtime.collectGroundTruthMetrics === undefined
      ? undefined
      : await runtime.collectGroundTruthMetrics({
          windowStart: startedAt,
          windowEnd: finishedAt,
          txHashSample: stressTxHashes.slice(0, 1_000),
          offeredCount: config.count,
          calibrationProofRef: null,
        });
  const fingerprint =
    groundTruth?.fingerprint ??
    (runtime.collectEnvironmentFingerprint === undefined
      ? undefined
      : await runtime.collectEnvironmentFingerprint({
          calibrationProofRef: null,
        }));
  const summary = parseE2EL2StressSummaryV1({
    schemaVersion: E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    runId: config.runId,
    status: interruptedReason === undefined ? "completed" : "interrupted",
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    classification: "closed_loop_smoke",
    rateSemantics: "burst_cycle_rate",
    burstCycleRatePerSecond: metrics.l2Admission.perSecond,
    mode: config.mode,
    measurementPolicy: measurementPolicyForConfig(config),
    requestedCount: config.count,
    notStartedCount,
    submittedCount,
    submissionFailedCount,
    acceptedCount,
    acceptanceNotObservedCount,
    acceptanceTimedOutCount,
    finalityTimedOutCount,
    observedCommittedCount,
    unknownFinalityCount,
    rejectedCount,
    concurrency: config.concurrency,
    finalityObserver: finalityObserverResult.summary,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    submissionDurationMs,
    durationMs,
    metrics,
    ...(groundTruth === undefined ? {} : { groundTruth }),
    ...(fingerprint === undefined ? {} : { fingerprint }),
    latencyMs: {
      submitP50: percentile(submitLatencies, 0.5),
      submitP95: percentile(submitLatencies, 0.95),
      acceptanceP50: percentile(acceptanceLatencies, 0.5),
      acceptanceP95: percentile(acceptanceLatencies, 0.95),
      commitP50: percentile(commitLatencies, 0.5),
      commitP95: percentile(commitLatencies, 0.95),
    },
    artifactPaths: {
      configJson: configJsonPath,
      eventsNdjson: eventsNdjsonPath,
      summaryJson: summaryJsonPath,
      summaryMarkdown: summaryMarkdownPath,
    },
    transactions: sortedTransactions,
  });

  await writeFile(summaryJsonPath, `${formatJson(summary)}\n`, "utf8");
  await writeFile(summaryMarkdownPath, renderStressSummaryMarkdown(summary), {
    encoding: "utf8",
  });
  await appendEvent(eventsNdjsonPath, {
    event: "stress_finished",
    at: finishedAt,
    summaryJsonPath,
    summaryMarkdownPath,
    submittedCount,
    acceptedCount,
    observedCommittedCount,
    rejectedCount,
    submissionFailedCount,
    acceptanceTimedOutCount,
    finalityTimedOutCount,
    unknownFinalityCount,
    notStartedCount,
    status: summary.status,
  });

  return {
    summary,
    configJsonPath,
    eventsNdjsonPath,
    summaryJsonPath,
    summaryMarkdownPath,
  };
};
