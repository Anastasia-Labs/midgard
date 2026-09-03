import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  REQUIRED_FRESH_E2E_STEP_IDS,
  REQUIRED_FRESH_TRANSACTION_LABELS,
  requiredFreshEvidence,
  requiredFreshStepAttemptQualityCounts,
  stressEvidenceFromSummary,
} from "../src/commands/e2e-finalize-summary.js";
import {
  E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
  type E2EL2StressSummary,
} from "../src/commands/e2e-stress-l2-throughput.js";
import { buildStressMetrics } from "../src/commands/stress-stage-metrics.js";
import {
  E2E_STEP_SCHEMA_VERSION,
  type StepStatus,
  type StepSummary,
  type TxObservation,
} from "../src/e2e/runner.js";
import {
  createE2ERunSummary,
  E2E_SUMMARY_SCHEMA_VERSION,
  parseE2ERunSummary,
  renderSummaryMarkdown,
  type TransactionEvidence,
  transactionEvidenceFromStepSummaries,
  updateE2ERunSummary,
  writeSummaryJsonAtomic,
  writeSummaryMarkdownAtomic,
} from "../src/e2e/summary.js";
import { createTrackedTempDirFactory } from "./helpers/temp-files.js";

const makeTempDir = createTrackedTempDirFactory("midgard-e2e-summary-");

const step = ({
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

const submittedObservation = ({
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

const requiredFreshSuccessSteps = (): readonly StepSummary[] =>
  REQUIRED_FRESH_E2E_STEP_IDS.map((id) => step({ id, status: "success" }));

const requiredFreshTransactions = (
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

const satisfiedHttp = [
  {
    label: "readyz",
    method: "GET",
    url: "http://127.0.0.1:3000/readyz",
    statusCode: 200,
    semanticStatus: "satisfied",
    source: "runner",
  },
] as const;

const stressSummary = (
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

describe("e2e run summary", () => {
  it("accepts only the exact V1 summary shape", () => {
    const summary = createE2ERunSummary({
      runId: "e2e-run-exact",
      mode: "fresh",
      now: new Date("2026-01-01T00:00:00.000Z"),
    });
    expect(parseE2ERunSummary(summary)).toEqual(summary);
    const { runId: _runId, ...missingRunId } = summary;
    expect(() => parseE2ERunSummary(missingRunId)).toThrow(
      "missing required field",
    );
    expect(() => parseE2ERunSummary({ ...summary, unexpected: true })).toThrow(
      "unknown field",
    );
    expect(() =>
      parseE2ERunSummary({
        ...summary,
        schemaVersion: "midgard-e2e-summary-v0",
      }),
    ).toThrow(E2E_SUMMARY_SCHEMA_VERSION);
    expect(() =>
      parseE2ERunSummary({
        ...summary,
        rawEvidence: [{ label: "logs", path: "logs/run", unexpected: true }],
      }),
    ).toThrow("unknown field");
    expect(() =>
      parseE2ERunSummary({
        ...summary,
        verdict: "success",
        nextSafeAction: "none_run_complete",
      }),
    ).toThrow("derived evidence or verdict is inconsistent");
    expect(() =>
      parseE2ERunSummary({
        ...summary,
        updatedAt: "2025-12-31T23:59:59.999Z",
      }),
    ).toThrow("derived evidence or verdict is inconsistent");
  });

  it("classifies a fully satisfied run as complete", () => {
    const base = createE2ERunSummary({
      runId: "e2e-run-1",
      mode: "fresh",
      now: new Date("2026-01-01T00:00:00.000Z"),
    });
    const summary = updateE2ERunSummary(
      base,
      {
        steps: [step({ id: "submit-deposit", status: "success" })],
        transactions: [
          {
            label: "l2-transfer-a",
            txHash: "aa".repeat(32),
            status: "committed",
            source: "tx-status",
          },
        ],
        http: [
          {
            label: "readyz",
            method: "GET",
            url: "http://127.0.0.1:3000/readyz",
            statusCode: 200,
            semanticStatus: "satisfied",
            source: "runner",
          },
        ],
        db: [
          {
            label: "finalization_residue",
            status: "satisfied",
            source: "psql",
            details: {},
          },
        ],
      },
      new Date("2026-01-01T00:01:00.000Z"),
    );

    expect(summary.schemaVersion).toBe(E2E_SUMMARY_SCHEMA_VERSION);
    expect(summary.verdict).toBe("success");
    expect(summary.cleanRunVerdict).toBe("success");
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.nextSafeAction).toBe("none_run_complete");
  });

  it("separates failed retry attempts from successful final functional gates", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-retried", mode: "fresh" }),
      {
        steps: [
          step({ id: "submit-deposit", status: "failed" }),
          step({ id: "submit-deposit", status: "success" }),
        ],
        http: [
          {
            label: "readyz",
            method: "GET",
            url: "http://127.0.0.1:3000/readyz",
            statusCode: 200,
            semanticStatus: "satisfied",
            source: "e2e-finalize-summary",
          },
        ],
        db: [
          {
            label: "finalization_residue",
            status: "satisfied",
            source: "postgres",
            details: {},
          },
        ],
        cleanRunGates: [
          {
            label: "required_fresh_step_attempt_quality",
            status: "failed",
            source: "e2e-run-step",
            details: {
              failed: "submit-deposit:failed:logs/submit-deposit.log",
            },
          },
        ],
        transactions: [
          {
            label: "deposit",
            txHash: "bb".repeat(32),
            status: "confirmed",
            source: "tx-status",
          },
        ],
      },
    );

    expect(summary.cleanRunVerdict).toBe("failed");
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.verdict).toBe("success");
    expect(summary.stepRetrySummary).toContainEqual(
      expect.objectContaining({
        stepId: "submit-deposit",
        attempts: 2,
        failedAttempts: 1,
        latestStatus: "success",
      }),
    );
  });

  it("routes timeout after submitted tx hash to reconciliation", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-2" }),
      {
        steps: [
          step({
            id: "submit-deposit",
            status: "timeout",
            txHashes: ["bb".repeat(32)],
            txObservations: [
              submittedObservation({
                stepId: "submit-deposit",
                txHash: "bb".repeat(32),
              }),
            ],
          }),
        ],
      },
    );

    expect(summary.verdict).toBe("blocked");
    expect(summary.nextSafeAction).toBe("reconcile_submitted_tx_before_rerun");
  });

  it("does not route timeout with only generic hashes to tx reconciliation", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-generic-timeout" }),
      {
        steps: [
          step({
            id: "await-root",
            status: "timeout",
            txHashes: ["aa".repeat(32)],
          }),
        ],
      },
    );

    expect(summary.verdict).toBe("blocked");
    expect(summary.nextSafeAction).toBe("investigate_unknown");
  });

  it("routes merge blockers to lease inspection", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-3" }),
      {
        steps: [step({ id: "merge", status: "success" })],
        http: [
          {
            label: "merge",
            method: "GET",
            url: "http://127.0.0.1:3000/merge",
            statusCode: 200,
            semanticStatus: "blocked",
            source: "runner",
          },
        ],
      },
    );

    expect(summary.verdict).toBe("blocked");
    expect(summary.nextSafeAction).toBe("inspect_state_queue_lease");
  });

  it("routes pending deposit projection to wait", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-4" }),
      {
        db: [
          {
            label: "deposit_projection",
            status: "pending",
            source: "psql",
            details: { inclusionTime: "2026-01-01T00:10:00.000Z" },
          },
        ],
      },
    );

    expect(summary.verdict).toBe("unknown");
    expect(summary.nextSafeAction).toBe("wait_until_deposit_projection_due");
  });

  it("fails the run when a required evidence gate fails", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-failed-gate" }),
      {
        steps: [step({ id: "readyz", status: "success" })],
        db: [
          {
            label: "required_fresh_steps",
            status: "failed",
            source: "e2e-run-step",
            details: { missing: "init-protocol" },
          },
        ],
      },
    );

    expect(summary.verdict).toBe("failed");
    expect(summary.cleanRunVerdict).toBe("success");
    expect(summary.functionalVerdict).toBe("failed");
    expect(summary.nextSafeAction).toBe("investigate_unknown");
  });

  it("requires all fresh state-changing steps and tx evidence", () => {
    const missing = requiredFreshEvidence({
      mode: "fresh",
      steps: [step({ id: "collect-final-evidence", status: "success" })],
      transactions: [],
    });

    expect(missing.db).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_steps",
        status: "failed",
        details: expect.objectContaining({
          missing: REQUIRED_FRESH_E2E_STEP_IDS.join(","),
        }),
      }),
    );
    expect(missing.db).toContainEqual(
      expect.objectContaining({
        label: "required_transaction_evidence",
        status: "failed",
        details: expect.objectContaining({
          missing: REQUIRED_FRESH_TRANSACTION_LABELS.join(","),
        }),
      }),
    );
    expect(missing.cleanRunGates).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_step_attempt_quality",
        status: "satisfied",
      }),
    );

    const satisfied = requiredFreshEvidence({
      mode: "fresh",
      steps: requiredFreshSuccessSteps(),
      transactions: requiredFreshTransactions(),
    });

    expect(satisfied.db.every((entry) => entry.status === "satisfied")).toBe(
      true,
    );
    expect(
      satisfied.cleanRunGates.every((entry) => entry.status === "satisfied"),
    ).toBe(true);
  });

  it("keeps required fresh coverage satisfied when a failed attempt is retried successfully", () => {
    const steps = [
      step({ id: "submit-deposit", status: "failed" }),
      ...requiredFreshSuccessSteps(),
    ];
    const evidence = requiredFreshEvidence({
      mode: "fresh",
      steps,
      transactions: requiredFreshTransactions(),
    });
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-dirty-success", mode: "fresh" }),
      {
        steps,
        transactions: requiredFreshTransactions(),
        http: satisfiedHttp,
        db: evidence.db,
        cleanRunGates: evidence.cleanRunGates,
      },
    );

    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_steps",
        status: "satisfied",
        details: expect.objectContaining({ missing: "" }),
      }),
    );
    expect(evidence.cleanRunGates).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_step_attempt_quality",
        status: "failed",
        details: expect.objectContaining({
          failedAttempts: "1",
          totalProblemAttempts: "1",
        }),
      }),
    );
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.cleanRunVerdict).toBe("failed");
    expect(summary.verdict).toBe("success");
    expect(summary.nextSafeAction).toBe("none_run_complete");
  });

  it("accepts identity-safe resume and retry aliases while preserving failed attempt quality", () => {
    const steps = [
      step({ id: "reference-scripts", status: "failed" }),
      ...requiredFreshSuccessSteps().filter(
        (entry) =>
          entry.id !== "reference-scripts" &&
          entry.id !== "init-protocol" &&
          entry.id !== "operator-lifecycle",
      ),
      step({ id: "reference-scripts-resume", status: "success" }),
      step({ id: "init-protocol-retry", status: "success" }),
      step({ id: "operator-activate-retry", status: "success" }),
    ];

    const evidence = requiredFreshEvidence({
      mode: "fresh",
      steps,
      transactions: requiredFreshTransactions(),
    });

    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_steps",
        status: "satisfied",
        details: expect.objectContaining({ missing: "" }),
      }),
    );
    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "required_transaction_evidence",
        status: "satisfied",
      }),
    );
    expect(evidence.cleanRunGates).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_step_attempt_quality",
        status: "failed",
        details: expect.objectContaining({
          failedAttempts: "1",
          failed: "reference-scripts:failed:logs/reference-scripts.log",
        }),
      }),
    );
  });

  it("blocks completion when a recovered signaled attempt has unresolved submitted tx risk", () => {
    const riskyTxHash = "bb".repeat(32);
    const steps = [
      step({
        id: "submit-deposit",
        status: "signaled",
        txObservations: [
          submittedObservation({
            stepId: "submit-deposit",
            txHash: riskyTxHash,
          }),
        ],
      }),
      ...requiredFreshSuccessSteps(),
    ];
    const evidence = requiredFreshEvidence({
      mode: "fresh",
      steps,
      transactions: requiredFreshTransactions(),
    });
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-signaled-risk", mode: "fresh" }),
      {
        steps,
        transactions: requiredFreshTransactions(),
        http: satisfiedHttp,
        db: evidence.db,
        cleanRunGates: evidence.cleanRunGates,
      },
    );

    expect(evidence.cleanRunGates).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_step_attempt_quality",
        status: "blocked",
        details: expect.objectContaining({
          signaledAttempts: "1",
          unreconciledAttempts: "1",
          submittedOrUnknownTransactions: "1",
        }),
      }),
    );
    expect(
      requiredFreshStepAttemptQualityCounts(evidence.cleanRunGates),
    ).toEqual(
      expect.objectContaining({
        status: "blocked",
        totalProblemAttempts: 1,
        signaledAttempts: 1,
        unreconciledAttempts: 1,
        submittedOrUnknownTransactions: 1,
      }),
    );
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.cleanRunVerdict).toBe("blocked");
    expect(summary.verdict).toBe("success");
    expect(summary.nextSafeAction).toBe("reconcile_submitted_tx_before_rerun");
  });

  it("lets later final transaction evidence satisfy a logical label after a rejected historical tx", () => {
    const rejectedTx = "ab".repeat(32);
    const confirmedTx = "cd".repeat(32);
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-rejected-then-confirmed" }),
      {
        transactions: [
          {
            label: "deposit",
            txHash: rejectedTx,
            status: "rejected",
            source: "historical-tx-status",
          },
          {
            label: "deposit",
            txHash: confirmedTx,
            status: "confirmed",
            source: "final-tx-status",
          },
        ],
      },
    );

    expect(summary.cleanRunVerdict).toBe("failed");
    expect(summary.finalFunctionalGates).toContainEqual(
      expect.objectContaining({
        label: "transaction:deposit",
        status: "satisfied",
        details: expect.objectContaining({
          txHash: confirmedTx,
          attempts: "2",
        }),
      }),
    );
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.verdict).toBe("success");
    expect(summary.nextSafeAction).toBe("none_run_complete");
  });

  it("counts required fresh tx evidence derived from step summaries", () => {
    const evidence = requiredFreshEvidence({
      mode: "fresh",
      steps: [
        step({
          id: "hub-oracle-nonce",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "hub-oracle-nonce",
              txHash: "01".repeat(32),
            }),
          ],
        }),
        step({ id: "reference-scripts", status: "success" }),
        step({
          id: "init-protocol",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "init-protocol",
              txHash: "02".repeat(32),
              field: "$.initTxHash",
            }),
          ],
        }),
        step({
          id: "operator-lifecycle",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "operator-lifecycle",
              txHash: "03".repeat(32),
              field: "$.registerTxHash",
            }),
            submittedObservation({
              stepId: "operator-lifecycle",
              txHash: "04".repeat(32),
              field: "$.activateTxHash",
            }),
          ],
        }),
        step({ id: "da-libp2p-bind-listen-preflight", status: "success" }),
        step({ id: "midgard-node-ready", status: "success" }),
        step({
          id: "submit-deposit",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "submit-deposit",
              txHash: "05".repeat(32),
            }),
          ],
        }),
        step({ id: "project-deposits", status: "success" }),
        step({
          id: "submit-l2-transfer-a",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "submit-l2-transfer-a",
              txHash: "06".repeat(32),
            }),
          ],
        }),
        step({
          id: "submit-l2-transfer-b",
          status: "success",
          txObservations: [
            submittedObservation({
              stepId: "submit-l2-transfer-b",
              txHash: "07".repeat(32),
            }),
          ],
        }),
        step({ id: "await-automatic-merge", status: "success" }),
      ],
      transactions: [
        {
          label: "header-commit-a",
          txHash: "08".repeat(32),
          status: "confirmed",
          source: "test",
        },
        {
          label: "header-commit-b",
          txHash: "09".repeat(32),
          status: "confirmed",
          source: "test",
        },
      ],
    });

    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "required_fresh_steps",
        status: "satisfied",
      }),
    );
    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "required_transaction_evidence",
        status: "satisfied",
        details: expect.objectContaining({ missing: "" }),
      }),
    );
  });

  it("does not promote prepared or generic hashes into transaction evidence", () => {
    const preparedHash = "cc".repeat(32);
    const submittedHash = "dd".repeat(32);
    const evidence = transactionEvidenceFromStepSummaries([
      step({
        id: "submit-l2-transfer-a",
        status: "success",
        txHashes: [preparedHash, submittedHash, "ee".repeat(32)],
        txObservations: [
          {
            txHash: preparedHash,
            role: "prepared",
            status: "prepared",
            source: "log:signed_tx_prepared",
            stepId: "submit-l2-transfer-a",
          },
          {
            txHash: submittedHash,
            role: "submitted",
            status: "submitted",
            source: "log:transaction_submitted",
            stepId: "submit-l2-transfer-a",
          },
        ],
      }),
    ]);

    expect(evidence).toEqual([
      {
        label: "l2-transfer-a",
        txHash: submittedHash,
        status: "submitted",
        source: "log:transaction_submitted:submit-l2-transfer-a",
      },
    ]);
  });

  it("does not treat submitted-only transaction evidence as a final functional success", () => {
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-submitted-only" }),
      {
        transactions: [
          {
            label: "deposit",
            txHash: "ff".repeat(32),
            status: "submitted",
            source: "log:transaction_submitted",
          },
        ],
      },
    );

    expect(summary.transactions).toHaveLength(1);
    expect(summary.finalFunctionalGates).toContainEqual(
      expect.objectContaining({
        label: "transaction:deposit",
        status: "pending",
      }),
    );
    expect(summary.functionalVerdict).toBe("unknown");
    expect(summary.verdict).toBe("unknown");
  });

  it("keeps rejected transaction evidence fail-closed over submitted evidence", () => {
    const txHash = "ab".repeat(32);
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-rejected-tx" }),
      {
        transactions: [
          {
            label: "deposit",
            txHash,
            status: "submitted",
            source: "log:transaction_submitted",
          },
          {
            label: "deposit",
            txHash,
            status: "rejected",
            source: "tx-status",
          },
        ],
      },
    );

    expect(summary.transactions).toEqual([
      {
        label: "deposit",
        txHash,
        status: "rejected",
        source: "tx-status",
      },
    ]);
    expect(summary.finalFunctionalGates).toContainEqual(
      expect.objectContaining({
        label: "transaction:deposit",
        status: "failed",
      }),
    );
    expect(summary.functionalVerdict).toBe("failed");
    expect(summary.verdict).toBe("failed");
  });

  it("keeps stress evidence empty when no stress summary is supplied", () => {
    expect(stressEvidenceFromSummary({})).toEqual({
      acceptedStressCount: 0,
      db: [],
      cleanRunGates: [],
      transactions: [],
      rawEvidence: [],
      notes: [],
    });
  });

  it("turns a successful stress summary into functional and artifact evidence", () => {
    const evidence = stressEvidenceFromSummary({
      stressSummary: stressSummary(),
      stressSummaryPath: "logs/e2e-run-stress/stress/summary.json",
    });
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-with-stress" }),
      {
        steps: [step({ id: "stress-l2-throughput", status: "success" })],
        db: evidence.db,
        cleanRunGates: evidence.cleanRunGates,
        transactions: evidence.transactions,
        rawEvidence: evidence.rawEvidence,
        notes: evidence.notes,
      },
    );

    expect(evidence.acceptedStressCount).toBe(2);
    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "stress_l2_acceptance",
        status: "satisfied",
        details: expect.objectContaining({
          rateSemantics: "burst_cycle_rate",
          burstCycleRatePerSecond: "2",
          interruptedReason: "",
        }),
      }),
    );
    expect(evidence.cleanRunGates).toContainEqual(
      expect.objectContaining({
        label: "stress_l2_acceptance_clean_run",
        status: "satisfied",
      }),
    );
    expect(evidence.transactions).toHaveLength(2);
    expect(evidence.rawEvidence).toEqual([
      {
        label: "stress-summary",
        path: "logs/e2e-run-stress/stress/summary.json",
      },
      {
        label: "stress-config",
        path: "logs/e2e-run-stress/stress/config.json",
      },
      {
        label: "stress-events",
        path: "logs/e2e-run-stress/stress/events.ndjson",
      },
      {
        label: "stress-summary-md",
        path: "logs/e2e-run-stress/stress/summary.md",
      },
    ]);
    expect(summary.functionalVerdict).toBe("success");
    expect(summary.cleanRunVerdict).toBe("success");
    expect(renderSummaryMarkdown(summary)).toContain(
      "stress_l2_acceptance accepted=2/2 l2AdmissionStatus=complete",
    );
    expect(renderSummaryMarkdown(summary)).toContain(
      "rateSemantics=burst_cycle_rate",
    );
  });

  it("blocks completion when stress leaves an acceptance timeout unresolved", () => {
    const evidence = stressEvidenceFromSummary({
      stressSummary: stressSummary({
        acceptedCount: 1,
        acceptanceTimedOutCount: 1,
        observedCommittedCount: 1,
        transactions: [
          {
            index: 0,
            phase: "stress",
            txHash: "ee".repeat(32),
            senderAddress: "addr_test_sender",
            destinationAddress: "addr_test_sender",
            selectedInputs: [`${"33".repeat(32)}#0`],
            submission: {
              status: "submitted",
              submittedAt: "2026-01-01T00:00:01.000Z",
            },
            acceptance: {
              status: "timeout",
              error: "Timed out waiting for /tx-status accepted",
            },
            finality: {
              status: "not_observed",
            },
            workerIndex: 0,
            walletSeedSource: "USER_SEED_PHRASE",
          },
        ],
      }),
      stressSummaryPath: "logs/e2e-run-stress/stress/summary.json",
    });
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-stress-timeout" }),
      {
        db: evidence.db,
        cleanRunGates: evidence.cleanRunGates,
        transactions: evidence.transactions,
      },
    );

    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "stress_l2_acceptance",
        status: "failed",
      }),
    );
    expect(summary.functionalVerdict).toBe("failed");
    expect(summary.nextSafeAction).toBe("reconcile_submitted_tx_before_rerun");
  });

  it("does not fail stress acceptance when only finality observation times out", () => {
    const evidence = stressEvidenceFromSummary({
      stressSummary: stressSummary({
        finalityTimedOutCount: 1,
        observedCommittedCount: 1,
        transactions: [
          {
            index: 0,
            phase: "stress",
            txHash: "ee".repeat(32),
            senderAddress: "addr_test_sender",
            destinationAddress: "addr_test_sender",
            selectedInputs: [`${"33".repeat(32)}#0`],
            submission: {
              status: "submitted",
              submittedAt: "2026-01-01T00:00:01.000Z",
            },
            acceptance: {
              status: "accepted",
              acceptedAt: "2026-01-01T00:00:01.500Z",
            },
            finality: {
              status: "timeout",
              error: "Timed out waiting for /tx-status committed",
            },
            workerIndex: 0,
            walletSeedSource: "USER_SEED_PHRASE",
          },
          stressSummary().transactions[1]!,
        ],
      }),
      stressSummaryPath: "logs/e2e-run-stress/stress/summary.json",
    });

    expect(evidence.acceptedStressCount).toBe(2);
    expect(evidence.db).toContainEqual(
      expect.objectContaining({
        label: "stress_l2_acceptance",
        status: "satisfied",
      }),
    );
    expect(evidence.transactions[0]).toMatchObject({
      status: "accepted",
    });
  });

  it("renders and writes summary artifacts atomically", async () => {
    const dir = await makeTempDir();
    const summary = updateE2ERunSummary(
      createE2ERunSummary({ runId: "e2e-run-5", mode: "resume" }),
      {
        steps: [step({ id: "readyz", status: "success" })],
        rawEvidence: [{ label: "node-log", path: "logs/node.log" }],
      },
    );
    const jsonPath = join(dir, "summary.json");
    const markdownPath = join(dir, "summary.md");

    await writeSummaryJsonAtomic(jsonPath, summary);
    await writeSummaryMarkdownAtomic(markdownPath, summary);

    await expect(readFile(jsonPath, "utf8")).resolves.toContain(
      '"schemaVersion": "midgard-e2e-summary-v1"',
    );
    const markdown = await readFile(markdownPath, "utf8");
    expect(markdown).toContain("# Midgard E2E Run Summary");
    expect(markdown).toContain("cleanRunVerdict");
    expect(markdown).toContain("## Clean Run Quality Gates");
    expect(markdown).toContain("## Step Retry Summary");
    expect(markdown).toContain("| readyz | success |");
    expect(renderSummaryMarkdown(summary)).toBe(markdown);
  });
});
