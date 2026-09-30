import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";

import {
  REQUIRED_FRESH_E2E_STEP_IDS,
  REQUIRED_FRESH_TRANSACTION_LABELS,
} from "../src/commands/e2e-finalize-summary.js";
import {
  E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
  type E2EL2StressSummary,
} from "../src/commands/e2e-stress-l2-throughput/index.js";
import { buildStressMetrics } from "../src/commands/stress-stage-metrics.js";
import {
  E2E_STEP_SCHEMA_VERSION,
  type StepStatus,
  type StepSummary,
  type TxObservation,
} from "../src/e2e/runner.js";
import { type TransactionEvidence } from "../src/e2e/summary.js";

export const makeTempDir = createTrackedTempDirFactory("midgard-e2e-summary-");

export const step = ({
  id,
  status,
  txHashes = [],
  txObservations = [],
}: {
  readonly id: string;
  readonly status: StepStatus;
  readonly txHashes?: readonly string[];
  readonly txObservations?: readonly TxObservation[];
}): StepSummary => ({
  schemaVersion: E2E_STEP_SCHEMA_VERSION,
  id,
  status,
  command: {
    command: "node",
    args: ["dist/index.js"],
    cwd: "/tmp",
    envKeys: [],
    envFiles: [],
    envInheritance: "process",
  },
  pid: 123,
  startedAt: "2026-01-01T00:00:00.000Z",
  finishedAt: "2026-01-01T00:00:01.000Z",
  durationMs: 1000,
  exitCode: status === "success" ? 0 : status === "failed" ? 1 : null,
  signal: status === "signaled" ? "SIGTERM" : null,
  timedOut: status === "timeout",
  rawLogPath: `logs/${id}.log`,
  observedTxHashes: txHashes,
  hashObservations: txHashes.map((hash) => ({
    hash,
    role: "unknown",
    source: "regex",
    stepId: id,
  })),
  txObservations,
  parsedJson: null,
  error: status === "success" ? null : "failed",
});

export const submittedObservation = ({
  stepId,
  txHash,
  field = "$.txHash",
}: {
  readonly stepId: string;
  readonly txHash: string;
  readonly field?: string;
}): TxObservation => ({
  txHash,
  role: "submitted",
  status: "submitted",
  source: "parsedJson",
  field,
  stepId,
});

export const requiredFreshSuccessSteps = (): readonly StepSummary[] =>
  REQUIRED_FRESH_E2E_STEP_IDS.map((id) => step({ id, status: "success" }));

export const requiredFreshTransactions = (
  extras: readonly TransactionEvidence[] = [],
): readonly TransactionEvidence[] => [
  ...REQUIRED_FRESH_TRANSACTION_LABELS.map<TransactionEvidence>(
    (label, index) => ({
      label,
      txHash: `${(index + 1).toString(16).padStart(2, "0")}`.repeat(32),
      status:
        label === "l2-transfer-a" || label === "l2-transfer-b"
          ? "committed"
          : "confirmed",
      source: "test",
    }),
  ),
  ...extras,
];

export const satisfiedHttp = [
  {
    label: "readyz",
    method: "GET",
    url: "http://127.0.0.1:3000/readyz",
    statusCode: 200,
    semanticStatus: "satisfied",
    source: "runner",
  },
] as const;

export const stressSummary = (
  patch: Partial<E2EL2StressSummary> = {},
): E2EL2StressSummary => {
  const base: Omit<E2EL2StressSummary, "metrics"> = {
    schemaVersion: E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    runId: "e2e-run-stress",
    status: "completed",
    loadModel: "closed-loop-smoke",
    workloadProfile: "production-end-user",
    classification: "closed_loop_smoke",
    rateSemantics: "burst_cycle_rate",
    burstCycleRatePerSecond: 2,
    mode: "serial-chain",
    measurementPolicy: {
      loadModel: "closed-loop-smoke",
      workloadProfile: "production-end-user",
      syntheticVsProduction: "production_end_user_path",
      advanceOn: "accepted",
      primaryStageMetric: "metrics.l2Admission.perSecond",
      finalityObservation: "post-submit-bounded",
      submissionWindowExcludesCommitDrain: true,
      fullFinalityRequiresDrainProof: true,
    },
    requestedCount: 2,
    notStartedCount: 0,
    submittedCount: 2,
    submissionFailedCount: 0,
    acceptedCount: 2,
    acceptanceNotObservedCount: 0,
    acceptanceTimedOutCount: 0,
    finalityTimedOutCount: 0,
    observedCommittedCount: 2,
    unknownFinalityCount: 0,
    rejectedCount: 0,
    concurrency: 1,
    finalityObserver: {
      mode: "post-submit-bounded",
      maxConcurrentRequests: 1,
      maxObservedConcurrentRequests: 1,
      observedTransactionCount: 2,
      pollRequestCount: 2,
      batchCount: 2,
      errorCount: 0,
    },
    startedAt: "2026-01-01T00:00:00.000Z",
    submissionFinishedAt: "2026-01-01T00:00:10.000Z",
    finishedAt: "2026-01-01T00:01:00.000Z",
    submissionDurationMs: 10_000,
    durationMs: 60_000,
    latencyMs: {
      submitP50: 100,
      submitP95: 150,
      acceptanceP50: 500,
      acceptanceP95: 800,
      commitP50: 1_000,
      commitP95: 1_500,
    },
    artifactPaths: {
      configJson: "logs/e2e-run-stress/stress/config.json",
      eventsNdjson: "logs/e2e-run-stress/stress/events.ndjson",
      summaryJson: "logs/e2e-run-stress/stress/summary.json",
      summaryMarkdown: "logs/e2e-run-stress/stress/summary.md",
    },
    transactions: [
      {
        index: 0,
        phase: "stress",
        txHash: "cc".repeat(32),
        senderAddress: "addr_test_sender",
        destinationAddress: "addr_test_sender",
        selectedInputs: [`${"11".repeat(32)}#0`],
        submission: {
          status: "submitted",
          submittedAt: "2026-01-01T00:00:01.000Z",
          durationMs: 100,
        },
        acceptance: {
          status: "accepted",
          acceptedAt: "2026-01-01T00:00:01.500Z",
          durationMs: 500,
        },
        finality: {
          status: "committed",
          committedAt: "2026-01-01T00:00:02.000Z",
          durationMs: 1_000,
        },
        workerIndex: 0,
        walletSeedSource: "USER_SEED_PHRASE",
      },
      {
        index: 1,
        phase: "stress",
        txHash: "dd".repeat(32),
        senderAddress: "addr_test_sender",
        destinationAddress: "addr_test_sender",
        selectedInputs: [`${"22".repeat(32)}#0`],
        submission: {
          status: "submitted",
          submittedAt: "2026-01-01T00:00:03.000Z",
          durationMs: 150,
        },
        acceptance: {
          status: "accepted",
          acceptedAt: "2026-01-01T00:00:03.500Z",
          durationMs: 500,
        },
        finality: {
          status: "committed",
          committedAt: "2026-01-01T00:00:04.000Z",
          durationMs: 1_500,
        },
        workerIndex: 0,
        walletSeedSource: "USER_SEED_PHRASE",
      },
    ],
  };
  const merged = {
    ...base,
    ...patch,
  };
  return {
    ...merged,
    metrics:
      patch.metrics ??
      buildStressMetrics({
        requestedCount: merged.requestedCount,
        submittedCount: merged.submittedCount,
        acceptedCount: merged.acceptedCount,
        observedCommittedCount: merged.observedCommittedCount,
        startedAt: merged.startedAt,
        submissionFinishedAt: merged.submissionFinishedAt,
        finishedAt: merged.finishedAt,
        transactions: merged.transactions,
      }),
  };
};
