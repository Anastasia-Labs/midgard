import { appendFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

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
import {
  buildOpenLoopPlacementProof,
  type NoOpCalibrationSummary,
  type OpenLoopCorpusPlan,
  type OpenLoopCorpusShape,
  type OpenLoopPlacementProof,
  type OpenLoopSubmitSummary,
  type OpenLoopWorkloadProfile,
  parseOpenLoopCorpusNdjson,
  planOpenLoopCorpus,
  runNoOpSubmitCalibration,
  runOpenLoopSubmitter,
} from "@/commands/stress-open-loop.js";
import {
  buildStressMetrics,
  flattenStressMetricRows,
  type StressFullFinalityDrainProof,
  type StressMetrics,
  type StressStageMetricDbSources,
} from "@/commands/stress-stage-metrics.js";
import {
  parseSubmitL2TransferConfig,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "@/commands/submit-l2-transfer.js";

export const E2E_L2_STRESS_SCHEMA_VERSION = "midgard-e2e-l2-stress-v2";

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
  | "client_overloaded";
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
  readonly schemaVersion: typeof E2E_L2_STRESS_SCHEMA_VERSION;
  readonly runId: string;
  readonly status: "completed" | "interrupted";
  readonly interruptedReason?: string;
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly corpusShape?: OpenLoopCorpusShape;
  readonly classification: E2EL2StressClassification;
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
  readonly pollIntervalMs: number;
  readonly submitRequestTimeoutMs: number;
  readonly acceptanceTimeoutMs: number;
  readonly commitObservationTimeoutMs: number;
  readonly finalityObserverMaxConcurrentRequests: number;
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
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly stressWalletSeedPhraseEnvs?: readonly string[];
  readonly l2Address?: string;
  readonly runId?: string;
  readonly outDir?: string;
  readonly pollIntervalMs?: string;
  readonly submitRequestTimeoutMs?: string;
  readonly acceptanceTimeoutMs?: string;
  readonly commitObservationTimeoutMs?: string;
  readonly finalityObserverMaxConcurrentRequests?: string;
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
  readonly collectAggregateObserverSample?: (input: {
    readonly at: string;
    readonly runId: string;
    readonly loadModel: E2EL2StressLoadModel;
  }) => Promise<unknown>;
  readonly fullFinalityDrainProof?: StressFullFinalityDrainProof;
};

const DEFAULT_COUNT = 25;
const DEFAULT_CONCURRENCY = 1;
const DEFAULT_LOVELACE = 1_000_000n;
const DEFAULT_POLL_INTERVAL_MS = 2_000;
const DEFAULT_SUBMIT_REQUEST_TIMEOUT_MS = 300_000;
const DEFAULT_ACCEPTANCE_TIMEOUT_MS = 600_000;
const DEFAULT_COMMIT_OBSERVATION_TIMEOUT_MS = 600_000;
const DEFAULT_FINALITY_OBSERVER_MAX_CONCURRENT_REQUESTS = 4;
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

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

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
  walletSeedPhrase,
  walletSeedPhraseEnv = DEFAULT_WALLET_SEED_ENV,
  stressWalletSeedPhraseEnvs = [],
  l2Address,
  runId,
  outDir,
  pollIntervalMs: rawPollIntervalMs,
  submitRequestTimeoutMs: rawSubmitRequestTimeoutMs,
  acceptanceTimeoutMs: rawAcceptanceTimeoutMs,
  commitObservationTimeoutMs: rawCommitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    rawFinalityObserverMaxConcurrentRequests,
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
  const pollIntervalMs = parsePositiveInteger(
    rawPollIntervalMs,
    "--poll-interval-ms",
    DEFAULT_POLL_INTERVAL_MS,
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
      ? stressWalletSeedPhraseEnvs.map((envName) =>
          resolveStressWallet({
            walletSeedPhraseEnv: envName,
            env,
            network,
          }),
        )
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
    submitRequestTimeoutMs,
    acceptanceTimeoutMs,
    commitObservationTimeoutMs,
    finalityObserverMaxConcurrentRequests,
    outDir: outDir?.trim() || join("logs", resolvedRunId, "stress"),
    network,
    allowUnsafeBounds,
    primaryWallet,
    stressWallets,
  };
};

const artifactConfig = (config: E2EL2StressConfig) => ({
  schemaVersion: E2E_L2_STRESS_SCHEMA_VERSION,
  runId: config.runId,
  loadModel: config.loadModel,
  workloadProfile: config.workloadProfile,
  corpusShape: config.corpusShape,
  mode: config.mode,
  measurementPolicy: measurementPolicyForConfig(config),
  count: config.count,
  concurrency: config.concurrency,
  lovelace: config.lovelace.toString(10),
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
  submitRequestTimeoutMs: config.submitRequestTimeoutMs,
  acceptanceTimeoutMs: config.acceptanceTimeoutMs,
  commitObservationTimeoutMs: config.commitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    config.finalityObserverMaxConcurrentRequests,
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

const appendEvent = async (
  eventsNdjsonPath: string,
  event: Readonly<Record<string, unknown>>,
): Promise<void> => {
  await appendFile(eventsNdjsonPath, `${JSON.stringify(event)}\n`, "utf8");
};

const percentile = (values: readonly number[], quantile: number): number => {
  if (values.length === 0) {
    return 0;
  }
  const sorted = [...values].sort((left, right) => left - right);
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(sorted.length * quantile) - 1),
  );
  return sorted[index]!;
};

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
    "| metric | status | count | missing | duration_s | rate_per_s | precision | source | notes |",
    "| --- | --- | ---: | ---: | ---: | ---: | --- | --- | --- |",
    ...metricRows.map(([label, metric]) => {
      const durationSeconds =
        metric.durationMs === null ? null : metric.durationMs / 1000;
      return `| ${label} | ${metric.status} | ${metric.count.toString()} | ${metric.missingCount.toString()} | ${formatMetricValue(durationSeconds)} | ${formatMetricValue(metric.perSecond)} | ${metric.precision} | ${metric.source} | ${formatMetricNotes(metric.notes)} |`;
    }),
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
    await sleepWithAbort(sleepImpl, config.pollIntervalMs, signal);
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
    pollIntervalMs: config.pollIntervalMs,
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
            outcome.polledAt.getTime() + outcome.entry.pollIntervalMs,
          pollIntervalMs: Math.min(
            outcome.entry.pollIntervalMs * 2,
            config.pollIntervalMs * 8,
          ),
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
        nextPollAtMs: outcome.polledAt.getTime() + outcome.entry.pollIntervalMs,
        pollIntervalMs: Math.min(
          outcome.entry.pollIntervalMs * 2,
          config.pollIntervalMs * 8,
        ),
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

  const corpusRows = parseOpenLoopCorpusNdjson(
    await readFile(config.corpusPath, "utf8"),
  );
  const corpus = planOpenLoopCorpus({
    rows: corpusRows,
    targetRateTps: config.targetRateTps,
    durationMs: config.openLoopDurationMs,
    warmupCount: config.openLoopWarmupCount,
    cooldownCount: config.openLoopCooldownCount,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
  });
  const placement = buildOpenLoopPlacementProof();
  const startedAtDate = now();
  const startedAt = startedAtDate.toISOString();
  await appendEvent(eventsNdjsonPath, {
    event: "stress_started",
    at: startedAt,
    runId: config.runId,
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    corpusSliceId: config.corpusSliceId,
    corpusSelectedTransactionCount: corpus.selectedTransactionCount,
    placement,
  });

  let calibration: NoOpCalibrationSummary | undefined;
  if (config.noOpCalibrationEndpoint !== undefined) {
    calibration = await runNoOpSubmitCalibration({
      endpoint: config.noOpCalibrationEndpoint,
      rows: corpus.rows,
      targetRateTps: config.targetRateTps,
      durationMs: config.noOpCalibrationDurationMs,
      maxInFlight: config.openLoopMaxInFlight,
      fetchImpl,
      signal,
    });
    await appendEvent(eventsNdjsonPath, {
      event: "stress.no_op_calibration.finished",
      at: now().toISOString(),
      calibration,
    });
    if (config.requireNoOpCalibration && !calibration.passed) {
      throw new Error("no-op submit calibration failed required gate");
    }
  } else if (config.requireNoOpCalibration) {
    throw new Error("no-op submit calibration was required but not configured");
  }

  const submitEndpoint = `${config.nodeEndpoint}/submit`;
  const { result: submitResult, observer } = await runAggregateObserverDuring({
    config,
    runtime,
    eventsNdjsonPath,
    sleepImpl,
    now,
    action: () =>
      runOpenLoopSubmitter({
        rows: corpus.rows,
        endpoint: submitEndpoint,
        targetRateTps: config.targetRateTps,
        maxInFlight: config.openLoopMaxInFlight,
        fetchImpl,
        signal,
      }),
  });
  const submissionFinishedAtDate = now();
  const submissionFinishedAt = submissionFinishedAtDate.toISOString();
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
  const classification = classifyOpenLoopRun({
    metrics,
    submission: submitResult.summary,
    calibration,
    placement,
    observer,
  });
  const summary: E2EL2StressSummary = {
    schemaVersion: E2E_L2_STRESS_SCHEMA_VERSION,
    runId: config.runId,
    status: signalWasAborted(signal) ? "interrupted" : "completed",
    ...(signalWasAborted(signal)
      ? { interruptedReason: abortReason(signal) }
      : {}),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    corpusShape: config.corpusShape,
    classification,
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
    },
    transactions,
  };
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
    await Promise.all(
      config.stressWallets.slice(0, config.concurrency).map(async (wallet) => {
        const utxos = await fetchUtxos(config.nodeEndpoint, wallet.address);
        await appendEvent(eventsNdjsonPath, {
          event: "stress_wallet_preflight",
          at: now().toISOString(),
          address: wallet.address,
          seedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
          utxoCount: utxos.length,
          spendableUtxoCount: spendableUtxosForLovelace(utxos, config.lovelace)
            .length,
          requiredLovelace: config.lovelace.toString(10),
        });
        if (spendableUtxosForLovelace(utxos, config.lovelace).length === 0) {
          throw new Error(
            `Stress wallet ${wallet.resolvedWalletSeedPhrase.resolvedFrom} has no spendable L2 UTxO with at least ${config.lovelace.toString(10)} lovelace at ${wallet.address}. Fund independent stress wallets before rerunning parallel stress.`,
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
  let dbMetricSources: StressStageMetricDbSources | undefined;
  if (runtime.collectStageMetricSources !== undefined) {
    const stressTxHashes = sortedTransactions.flatMap((tx) =>
      tx.txHash === null ? [] : [tx.txHash],
    );
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
  const summary: E2EL2StressSummary = {
    schemaVersion: E2E_L2_STRESS_SCHEMA_VERSION,
    runId: config.runId,
    status: interruptedReason === undefined ? "completed" : "interrupted",
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    classification: "closed_loop_smoke",
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
  };

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
