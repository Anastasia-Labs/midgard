import { appendFile, mkdir, writeFile } from "node:fs/promises";
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
  parseSubmitL2TransferConfig,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "@/commands/submit-l2-transfer.js";
import {
  buildStressMetrics,
  flattenStressMetricRows,
  type StressFullFinalityDrainProof,
  type StressMetrics,
  type StressStageMetricDbSources,
} from "@/commands/stress-stage-metrics.js";

export const E2E_L2_STRESS_SCHEMA_VERSION = "midgard-e2e-l2-stress-v2";

export type E2EL2StressMode = "serial-chain" | "parallel-fanout";
export type E2EL2StressMeasurementPolicy = {
  readonly advanceOn: "accepted";
  readonly primaryStageMetric: "metrics.l2Admission.perSecond";
  readonly finalityObservation: "post-submit-bounded";
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
  readonly mode: "post-submit-bounded";
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
  readonly mode: E2EL2StressMode;
  readonly measurementPolicy: E2EL2StressMeasurementPolicy;
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
  readonly mode: E2EL2StressMode;
  readonly count: number;
  readonly concurrency: number;
  readonly lovelace: bigint;
  readonly nodeEndpoint: string;
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
  readonly mode?: string;
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

export const E2E_L2_STRESS_MEASUREMENT_POLICY: E2EL2StressMeasurementPolicy = {
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
  mode: rawMode,
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
  const mode = parseMode(rawMode);
  const count = parsePositiveInteger(rawCount, "--count", DEFAULT_COUNT);
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
    mode === "serial-chain"
      ? resolveStressWallet({
          walletSeedPhrase,
          walletSeedPhraseEnv,
          env,
          network,
        })
      : undefined;
  const stressWallets = stressWalletSeedPhraseEnvs.map((envName) =>
    resolveStressWallet({
      walletSeedPhraseEnv: envName,
      env,
      network,
    }),
  );
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
  if (concurrency > 1 && mode !== "parallel-fanout") {
    throw new Error(
      "--concurrency > 1 requires --mode parallel-fanout and independent stress wallet seed env vars.",
    );
  }
  if (mode === "parallel-fanout" && stressWallets.length < concurrency) {
    throw new Error(
      `--mode parallel-fanout requires at least ${concurrency.toString()} independent --stress-wallet-seed-phrase-env values with spendable L2 UTxOs; no submissions were made.`,
    );
  }

  return {
    runId: resolvedRunId,
    mode,
    count,
    concurrency,
    lovelace,
    nodeEndpoint: normalizedEndpoint,
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
  mode: config.mode,
  measurementPolicy: E2E_L2_STRESS_MEASUREMENT_POLICY,
  count: config.count,
  concurrency: config.concurrency,
  lovelace: config.lovelace.toString(10),
  nodeEndpoint: config.nodeEndpoint,
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
    config.mode === "parallel-fanout"
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
    `- mode: ${summary.mode}`,
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

export const runE2EL2StressThroughput = async (
  config: E2EL2StressConfig,
  runtime: E2EL2StressRuntime,
): Promise<E2EL2StressRunResult> => {
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
    mode: config.mode,
    measurementPolicy: E2E_L2_STRESS_MEASUREMENT_POLICY,
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
