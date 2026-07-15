import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { defaultMidgardNodeEndpoint } from "@/commands/command-utils.js";
import {
  E2E_L2_STRESS_SCHEMA_VERSION,
  type E2EL2StressSummary,
  type E2EL2StressTransaction,
} from "@/commands/e2e-stress-l2-throughput.js";
import type { StressMetricWindow } from "@/commands/stress-stage-metrics.js";
import { E2E_STEP_SCHEMA_VERSION, type StepSummary } from "@/e2e/runner.js";
import {
  type CleanRunGate,
  createE2ERunSummary,
  type DbEvidence,
  mergeTransactionEvidence,
  type RawEvidenceRef,
  type RunVerdict,
  type TransactionEvidence,
  transactionEvidenceFromStepSummaries,
  updateE2ERunSummary,
  writeSummaryJsonAtomic,
  writeSummaryMarkdownAtomic,
} from "@/e2e/summary.js";
import type { Database } from "@/services/database.js";

export type FinalizeSummaryOptions = {
  readonly outDir?: string;
  readonly runId?: string;
  readonly mode?: "attach" | "resume" | "fresh" | "unknown";
  readonly nodeUrl?: string;
  readonly adminApiKey?: string;
  readonly nodeLogPath?: string;
  readonly stepSummaryPaths?: readonly string[];
  readonly transactions?: readonly TransactionEvidence[];
  readonly stressSummaryPath?: string;
};

export type FinalizeSummaryResult = {
  readonly summaryJsonPath: string;
  readonly summaryMarkdownPath: string;
  readonly verdict: string;
  readonly functionalVerdict: RunVerdict;
  readonly cleanRunVerdict: RunVerdict;
  readonly nextSafeAction: string;
  readonly requiredFreshStepAttemptQuality: RequiredFreshStepAttemptQualityCounts;
};

export type RequiredFreshStepAttemptQualityCounts = {
  readonly status: CleanRunGate["status"] | "not_applicable";
  readonly totalProblemAttempts: number;
  readonly failedAttempts: number;
  readonly timeoutAttempts: number;
  readonly signaledAttempts: number;
  readonly runnerErrorAttempts: number;
  readonly unreconciledAttempts: number;
  readonly submittedOrUnknownTransactions: number;
  readonly rejectedTransactions: number;
};

type HttpProbe = {
  readonly statusCode: number;
  readonly body: unknown;
};

type CountRow = {
  readonly label: string;
  readonly count: number | bigint | string;
};

const timestampForPath = (date = new Date()): string =>
  date
    .toISOString()
    .replaceAll(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");

const countValue = (value: number | bigint | string | undefined): bigint => {
  if (value === undefined) {
    return 0n;
  }
  return typeof value === "bigint" ? value : BigInt(value);
};

const fetchJson = async (
  url: string,
  headers: Readonly<Record<string, string>> = {},
): Promise<HttpProbe> => {
  const response = await fetch(url, { headers });
  let body: unknown = null;
  try {
    body = await response.json();
  } catch {
    body = await response.text();
  }
  return {
    statusCode: response.status,
    body,
  };
};

const hasEmptyHeaders = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  Array.isArray((body as { readonly headers?: unknown }).headers) &&
  (body as { readonly headers: readonly unknown[] }).headers.length === 0;

const isReady = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  (body as { readonly ready?: unknown }).ready === true;

const collectorStep = ({
  startedAt,
  finishedAt,
  rawLogPath,
}: {
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly rawLogPath: string;
}): StepSummary => ({
  schemaVersion: E2E_STEP_SCHEMA_VERSION,
  id: "collect-final-evidence",
  status: "success",
  command: {
    command: "midgard-node e2e-finalize-summary",
    args: [],
    cwd: process.cwd(),
    envKeys: [],
    envFiles: [],
    envInheritance: "process",
  },
  pid: process.pid,
  startedAt,
  finishedAt,
  durationMs: Math.max(0, Date.parse(finishedAt) - Date.parse(startedAt)),
  exitCode: 0,
  signal: null,
  timedOut: false,
  rawLogPath,
  observedTxHashes: [],
  hashObservations: [],
  txObservations: [],
  parsedJson: null,
  error: null,
});

const loadStepSummaries = async (
  paths: readonly string[],
): Promise<readonly StepSummary[]> =>
  Promise.all(
    paths.map(
      async (path) => JSON.parse(await readFile(path, "utf8")) as StepSummary,
    ),
  );

export const loadStressSummary = async (
  path: string,
): Promise<E2EL2StressSummary> => {
  const parsed = JSON.parse(await readFile(path, "utf8")) as {
    readonly schemaVersion?: unknown;
  };
  if (parsed.schemaVersion !== E2E_L2_STRESS_SCHEMA_VERSION) {
    throw new Error(
      `${path} is not a ${E2E_L2_STRESS_SCHEMA_VERSION} artifact.`,
    );
  }
  return parsed as E2EL2StressSummary;
};

const stressTxPhaseToEvidenceStatus = (
  tx: E2EL2StressTransaction,
): TransactionEvidence["status"] => {
  if (
    tx.acceptance.status === "rejected" ||
    tx.finality.status === "rejected"
  ) {
    return "rejected";
  }
  if (tx.finality.status === "committed") {
    return "committed";
  }
  if (tx.acceptance.status === "accepted") {
    return "accepted";
  }
  if (tx.txHash !== null && tx.submission.status === "submitted") {
    return "submitted";
  }
  return "unknown";
};

const stressMetricDetails = (
  metric: StressMetricWindow,
  prefix: string,
): Readonly<Record<string, string>> => ({
  [`${prefix}Status`]: metric.status,
  [`${prefix}Count`]: metric.count.toString(),
  [`${prefix}MissingCount`]: metric.missingCount.toString(),
  [`${prefix}StartedAt`]: metric.startedAt ?? "",
  [`${prefix}FinishedAt`]: metric.finishedAt ?? "",
  [`${prefix}DurationMs`]: metric.durationMs?.toString() ?? "",
  [`${prefix}PerSecond`]: metric.perSecond?.toString() ?? "",
  [`${prefix}Source`]: metric.source,
  [`${prefix}Precision`]: metric.precision,
  [`${prefix}Notes`]: metric.notes.join(","),
});

const fullFinalityDrainRequested = (metric: StressMetricWindow): boolean =>
  !metric.notes.includes("full_finality_drain_not_requested");

export const stressEvidenceFromSummary = ({
  stressSummary,
  stressSummaryPath,
}: {
  readonly stressSummary?: E2EL2StressSummary;
  readonly stressSummaryPath?: string;
}): {
  readonly acceptedStressCount: number;
  readonly db: readonly DbEvidence[];
  readonly cleanRunGates: readonly CleanRunGate[];
  readonly transactions: readonly TransactionEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
} => {
  if (stressSummary === undefined) {
    return {
      acceptedStressCount: 0,
      db: [],
      cleanRunGates: [],
      transactions: [],
      rawEvidence: [],
      notes: [],
    };
  }

  const stressTransactions = stressSummary.transactions.filter(
    (tx) => tx.phase === "stress",
  );
  const unresolvedCount = stressTransactions.filter(
    (tx) =>
      tx.txHash !== null &&
      tx.submission.status === "submitted" &&
      tx.acceptance.status !== "accepted" &&
      tx.acceptance.status !== "rejected",
  ).length;
  const acceptanceRejectedCount = stressTransactions.filter(
    (tx) => tx.acceptance.status === "rejected",
  ).length;
  const l2Admission = stressSummary.metrics.l2Admission;
  const fullFinality = stressSummary.metrics.fullFinality;
  const failedAcceptanceCount =
    acceptanceRejectedCount +
    stressSummary.submissionFailedCount +
    stressSummary.acceptanceTimedOutCount;
  const allRequestedAccepted =
    stressSummary.requestedCount > 0 &&
    l2Admission.count >= stressSummary.requestedCount;
  const stressGateSatisfied =
    allRequestedAccepted &&
    failedAcceptanceCount === 0 &&
    unresolvedCount === 0;
  const cleanRunStatus: CleanRunGate["status"] =
    unresolvedCount > 0
      ? "blocked"
      : failedAcceptanceCount > 0 || !allRequestedAccepted
        ? "failed"
        : "satisfied";
  const summarySource = stressSummaryPath ?? "e2e-stress-l2-throughput";

  return {
    acceptedStressCount: l2Admission.count,
    db: [
      {
        label: "stress_l2_acceptance",
        status: stressGateSatisfied ? "satisfied" : "failed",
        source: "e2e-stress-l2-throughput",
        details: {
          requested: stressSummary.requestedCount.toString(),
          submitted: stressSummary.submittedCount.toString(),
          submissionFailed: stressSummary.submissionFailedCount.toString(),
          accepted: l2Admission.count.toString(),
          artifactAccepted: stressSummary.acceptedCount.toString(),
          acceptanceTimedOut: stressSummary.acceptanceTimedOutCount.toString(),
          acceptanceRejected: acceptanceRejectedCount.toString(),
          observedCommitted: stressSummary.observedCommittedCount.toString(),
          finalityTimedOut: stressSummary.finalityTimedOutCount.toString(),
          rejected: stressSummary.rejectedCount.toString(),
          unresolved: unresolvedCount.toString(),
          rateSemantics: stressSummary.rateSemantics,
          burstCycleRatePerSecond:
            stressSummary.burstCycleRatePerSecond?.toString() ?? "",
          interruptedReason: stressSummary.interruptedReason ?? "",
          ...stressMetricDetails(
            stressSummary.metrics.clientSubmission,
            "clientSubmission",
          ),
          ...stressMetricDetails(l2Admission, "l2Admission"),
          ...stressMetricDetails(
            stressSummary.metrics.immutableObservation,
            "immutableObservation",
          ),
          ...stressMetricDetails(fullFinality, "fullFinality"),
          advanceOn: stressSummary.measurementPolicy.advanceOn,
          primaryStageMetric:
            stressSummary.measurementPolicy.primaryStageMetric,
          finalityObservation:
            stressSummary.measurementPolicy.finalityObservation,
          fullFinalityRequiresDrainProof:
            stressSummary.measurementPolicy.fullFinalityRequiresDrainProof.toString(),
          submissionWindowExcludesCommitDrain:
            stressSummary.measurementPolicy.submissionWindowExcludesCommitDrain.toString(),
          summaryPath: stressSummaryPath ?? "",
        },
      },
      ...(fullFinalityDrainRequested(fullFinality)
        ? [
            {
              label: "stress_l2_full_finality",
              status:
                fullFinality.status === "complete"
                  ? ("satisfied" as const)
                  : ("failed" as const),
              source: "e2e-stress-l2-throughput",
              details: {
                requested: stressSummary.requestedCount.toString(),
                accepted: l2Admission.count.toString(),
                ...stressMetricDetails(fullFinality, "fullFinality"),
              },
            },
          ]
        : []),
    ],
    cleanRunGates: [
      {
        label: "stress_l2_acceptance_clean_run",
        status: cleanRunStatus,
        source: "e2e-stress-l2-throughput",
        details: {
          requested: stressSummary.requestedCount.toString(),
          accepted: l2Admission.count.toString(),
          artifactAccepted: stressSummary.acceptedCount.toString(),
          submissionFailed: stressSummary.submissionFailedCount.toString(),
          acceptanceTimedOut: stressSummary.acceptanceTimedOutCount.toString(),
          acceptanceRejected: acceptanceRejectedCount.toString(),
          observedCommitted: stressSummary.observedCommittedCount.toString(),
          finalityTimedOut: stressSummary.finalityTimedOutCount.toString(),
          rejected: stressSummary.rejectedCount.toString(),
          unresolved: unresolvedCount.toString(),
          interruptedReason: stressSummary.interruptedReason ?? "",
        },
      },
    ],
    transactions: stressTransactions.map((tx) => ({
      label: `stress-l2-${tx.index.toString()}`,
      txHash: tx.txHash ?? `unknown-stress-l2-${tx.index.toString()}`,
      status: stressTxPhaseToEvidenceStatus(tx),
      source: summarySource,
    })),
    rawEvidence:
      stressSummaryPath === undefined
        ? []
        : [
            { label: "stress-summary", path: stressSummaryPath },
            {
              label: "stress-config",
              path: stressSummary.artifactPaths.configJson,
            },
            {
              label: "stress-events",
              path: stressSummary.artifactPaths.eventsNdjson,
            },
            {
              label: "stress-summary-md",
              path: stressSummary.artifactPaths.summaryMarkdown,
            },
          ],
    notes: [
      `stress_l2_acceptance accepted=${l2Admission.count.toString()}/${stressSummary.requestedCount.toString()} l2AdmissionStatus=${l2Admission.status} l2AdmissionRatePerSecond=${l2Admission.perSecond?.toString() ?? "unavailable"} rateSemantics=${stressSummary.rateSemantics} immutableObservation=${stressSummary.metrics.immutableObservation.count.toString()}/${l2Admission.count.toString()} fullFinality=${fullFinality.status}`,
    ],
  };
};

export const REQUIRED_FRESH_E2E_STEP_IDS = [
  "hub-oracle-nonce",
  "reference-scripts",
  "init-protocol",
  "operator-lifecycle",
  "da-libp2p-bind-listen-preflight",
  "midgard-node-ready",
  "submit-deposit",
  "project-deposits",
  "submit-l2-transfer-a",
  "submit-l2-transfer-b",
  "await-automatic-merge",
] as const;

export const REQUIRED_FRESH_TRANSACTION_LABELS = [
  "hub-oracle-nonce",
  "init",
  "operator-registration",
  "operator-activation",
  "deposit",
  "l2-transfer-a",
  "l2-transfer-b",
  "header-commit-a",
  "header-commit-b",
] as const;

const REQUIRED_FRESH_E2E_STEP_ID_SET = new Set<string>(
  REQUIRED_FRESH_E2E_STEP_IDS,
);

const REQUIRED_FRESH_STEP_SUCCESS_ALIASES = new Map<string, string>([
  ["reference-scripts-resume", "reference-scripts"],
  ["init-protocol-retry", "init-protocol"],
]);

const REQUIRED_FRESH_STEP_ATTEMPT_ALIASES = new Map<string, string>([
  ...REQUIRED_FRESH_STEP_SUCCESS_ALIASES,
  ["operator-activate-retry", "operator-lifecycle"],
]);

const canonicalFreshStepSuccessId = (stepId: string): string =>
  REQUIRED_FRESH_STEP_SUCCESS_ALIASES.get(stepId) ?? stepId;

const canonicalFreshStepAttemptId = (stepId: string): string =>
  REQUIRED_FRESH_STEP_ATTEMPT_ALIASES.get(stepId) ?? stepId;

const REQUIRED_FRESH_TRANSACTION_STEP_IDS = new Set<string>([
  "hub-oracle-nonce",
  "reference-scripts",
  "init-protocol",
  "operator-lifecycle",
  "submit-deposit",
  "submit-l2-transfer-a",
  "submit-l2-transfer-b",
]);

const EVIDENCE_TRANSACTION_STATUSES = new Set<TransactionEvidence["status"]>([
  "submitted",
  "confirmed",
  "committed",
]);

const RECONCILED_TRANSACTION_STATUSES = new Set<TransactionEvidence["status"]>([
  "confirmed",
  "committed",
  "rejected",
]);

const UNRESOLVED_TRANSACTION_STATUSES = new Set<TransactionEvidence["status"]>([
  "submitted",
  "unknown",
]);

type RequiredFreshEvidenceResult = {
  readonly db: readonly DbEvidence[];
  readonly cleanRunGates: readonly CleanRunGate[];
};

const hasReconciledTransactionHash = (
  transactions: readonly TransactionEvidence[],
  txHash: string,
): boolean =>
  transactions.some(
    (tx) =>
      tx.txHash.toLowerCase() === txHash.toLowerCase() &&
      RECONCILED_TRANSACTION_STATUSES.has(tx.status),
  );

const stepHasUnresolvedTxObservation = ({
  step,
  transactions,
}: {
  readonly step: StepSummary;
  readonly transactions: readonly TransactionEvidence[];
}): boolean =>
  step.txObservations.some(
    (observation) =>
      (observation.role === "submitted" || observation.role === "unknown") &&
      !hasReconciledTransactionHash(transactions, observation.txHash),
  );

const stepHasUnknownInterruptedSubmitRisk = (step: StepSummary): boolean =>
  (step.status === "timeout" || step.status === "signaled") &&
  REQUIRED_FRESH_TRANSACTION_STEP_IDS.has(
    canonicalFreshStepAttemptId(step.id),
  ) &&
  step.txObservations.length === 0;

const formatStepAttempts = (steps: readonly StepSummary[]): string =>
  steps.map((step) => `${step.id}:${step.status}:${step.rawLogPath}`).join(",");

const formatTransactions = (
  transactions: readonly TransactionEvidence[],
): string =>
  transactions.map((tx) => `${tx.label}:${tx.status}:${tx.txHash}`).join(",");

const requiredFreshStepAttemptQualityGate = ({
  attempts,
  transactions,
}: {
  readonly attempts: readonly StepSummary[];
  readonly transactions: readonly TransactionEvidence[];
}): CleanRunGate => {
  const problemAttempts = attempts.filter((step) => step.status !== "success");
  const failedAttempts = problemAttempts.filter(
    (step) => step.status === "failed",
  );
  const timeoutAttempts = problemAttempts.filter(
    (step) => step.status === "timeout",
  );
  const signaledAttempts = problemAttempts.filter(
    (step) => step.status === "signaled",
  );
  const runnerErrorAttempts = problemAttempts.filter(
    (step) => step.status === "runner_error",
  );
  const unreconciledAttempts = problemAttempts.filter(
    (step) =>
      stepHasUnresolvedTxObservation({ step, transactions }) ||
      stepHasUnknownInterruptedSubmitRisk(step),
  );
  const submittedOrUnknownTransactions = transactions.filter(
    (tx) =>
      UNRESOLVED_TRANSACTION_STATUSES.has(tx.status) &&
      !hasReconciledTransactionHash(transactions, tx.txHash),
  );
  const rejectedTransactions = transactions.filter(
    (tx) => tx.status === "rejected",
  );
  const status: CleanRunGate["status"] =
    unreconciledAttempts.length > 0 || submittedOrUnknownTransactions.length > 0
      ? "blocked"
      : timeoutAttempts.length > 0 || signaledAttempts.length > 0
        ? "interrupted"
        : failedAttempts.length > 0 ||
            runnerErrorAttempts.length > 0 ||
            rejectedTransactions.length > 0
          ? "failed"
          : "satisfied";

  return {
    label: "required_fresh_step_attempt_quality",
    status,
    source: "e2e-run-step",
    details: {
      totalProblemAttempts: problemAttempts.length.toString(),
      failedAttempts: failedAttempts.length.toString(),
      timeoutAttempts: timeoutAttempts.length.toString(),
      signaledAttempts: signaledAttempts.length.toString(),
      runnerErrorAttempts: runnerErrorAttempts.length.toString(),
      unreconciledAttempts: unreconciledAttempts.length.toString(),
      submittedOrUnknownTransactions:
        submittedOrUnknownTransactions.length.toString(),
      rejectedTransactions: rejectedTransactions.length.toString(),
      failed: formatStepAttempts(failedAttempts),
      timeout: formatStepAttempts(timeoutAttempts),
      signaled: formatStepAttempts(signaledAttempts),
      runnerError: formatStepAttempts(runnerErrorAttempts),
      unreconciled: formatStepAttempts(unreconciledAttempts),
      submittedOrUnknown: formatTransactions(submittedOrUnknownTransactions),
      rejected: formatTransactions(rejectedTransactions),
    },
  };
};

export const requiredFreshStepAttemptQualityCounts = (
  cleanRunGates: readonly CleanRunGate[],
): RequiredFreshStepAttemptQualityCounts => {
  const gate = cleanRunGates.find(
    (entry) => entry.label === "required_fresh_step_attempt_quality",
  );
  if (gate === undefined) {
    return {
      status: "not_applicable",
      totalProblemAttempts: 0,
      failedAttempts: 0,
      timeoutAttempts: 0,
      signaledAttempts: 0,
      runnerErrorAttempts: 0,
      unreconciledAttempts: 0,
      submittedOrUnknownTransactions: 0,
      rejectedTransactions: 0,
    };
  }
  const count = (key: string): number => Number(gate.details[key] ?? 0);
  return {
    status: gate.status,
    totalProblemAttempts: count("totalProblemAttempts"),
    failedAttempts: count("failedAttempts"),
    timeoutAttempts: count("timeoutAttempts"),
    signaledAttempts: count("signaledAttempts"),
    runnerErrorAttempts: count("runnerErrorAttempts"),
    unreconciledAttempts: count("unreconciledAttempts"),
    submittedOrUnknownTransactions: count("submittedOrUnknownTransactions"),
    rejectedTransactions: count("rejectedTransactions"),
  };
};

export const requiredFreshEvidence = ({
  mode,
  steps,
  transactions,
}: {
  readonly mode: FinalizeSummaryOptions["mode"];
  readonly steps: readonly StepSummary[];
  readonly transactions: readonly TransactionEvidence[];
}): RequiredFreshEvidenceResult => {
  if (mode !== "fresh") {
    return { db: [], cleanRunGates: [] };
  }
  const requiredStepAttempts = steps.filter((step) =>
    REQUIRED_FRESH_E2E_STEP_ID_SET.has(canonicalFreshStepAttemptId(step.id)),
  );
  const successfulStepIds = new Set(
    requiredStepAttempts
      .filter((step) => step.status === "success")
      .map((step) => canonicalFreshStepSuccessId(step.id))
      .filter((stepId) => REQUIRED_FRESH_E2E_STEP_ID_SET.has(stepId)),
  );
  const allTransactionEvidence = mergeTransactionEvidence([
    ...transactions,
    ...transactionEvidenceFromStepSummaries(steps),
  ]);
  const txLabels = new Set(
    allTransactionEvidence
      .filter((tx) => EVIDENCE_TRANSACTION_STATUSES.has(tx.status))
      .map((tx) => tx.label),
  );
  const confirmedTxLabels = new Set(
    allTransactionEvidence
      .filter((tx) => tx.status === "confirmed" || tx.status === "committed")
      .map((tx) => tx.label),
  );
  if (
    confirmedTxLabels.has("operator-registration") &&
    confirmedTxLabels.has("operator-activation")
  ) {
    successfulStepIds.add("operator-lifecycle");
  }
  const missingStepIds = REQUIRED_FRESH_E2E_STEP_IDS.filter(
    (stepId) => !successfulStepIds.has(stepId),
  );
  const missingTransactionLabels = REQUIRED_FRESH_TRANSACTION_LABELS.filter(
    (label) => !txLabels.has(label),
  );
  return {
    db: [
      {
        label: "required_fresh_steps",
        status: missingStepIds.length === 0 ? "satisfied" : "failed",
        source: "e2e-run-step",
        details: {
          required: REQUIRED_FRESH_E2E_STEP_IDS.join(","),
          successful: REQUIRED_FRESH_E2E_STEP_IDS.filter((stepId) =>
            successfulStepIds.has(stepId),
          ).join(","),
          missing: missingStepIds.join(","),
        },
      },
      {
        label: "required_transaction_evidence",
        status: missingTransactionLabels.length === 0 ? "satisfied" : "failed",
        source: "e2e-finalize-summary",
        details: {
          required: REQUIRED_FRESH_TRANSACTION_LABELS.join(","),
          missing: missingTransactionLabels.join(","),
        },
      },
    ],
    cleanRunGates: [
      requiredFreshStepAttemptQualityGate({
        attempts: requiredStepAttempts,
        transactions: allTransactionEvidence,
      }),
    ],
  };
};

const collectDbCounts = (): Effect.Effect<
  ReadonlyMap<string, bigint>,
  never,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<CountRow>`
      SELECT 'deposits_consumed' AS label, COUNT(*) AS count
        FROM deposits_utxos WHERE status = 'consumed'
      UNION ALL
      SELECT 'tx_admissions_accepted' AS label, COUNT(*) AS count
        FROM tx_admissions WHERE status = 'accepted'
      UNION ALL
      SELECT 'pending_finalizations_finalized' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status = 'finalized'
      UNION ALL
      SELECT 'pending_finalization_tx_headers' AS label, COUNT(DISTINCT header_hash) AS count
        FROM pending_block_finalization_txs
      UNION ALL
      SELECT 'pending_finalizations_finalized_tx_headers' AS label, COUNT(DISTINCT f.header_hash) AS count
        FROM pending_block_finalizations f
        JOIN pending_block_finalization_txs t USING (header_hash)
        WHERE f.status = 'finalized'
      UNION ALL
      SELECT 'pending_finalizations_unfinished' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status <> 'finalized'
      UNION ALL
      SELECT 'mempool' AS label, COUNT(*) AS count FROM mempool
      UNION ALL
      SELECT 'processed_mempool' AS label, COUNT(*) AS count FROM processed_mempool
      UNION ALL
      SELECT 'blocks' AS label, COUNT(*) AS count FROM blocks
      UNION ALL
      SELECT 'immutable' AS label, COUNT(*) AS count FROM immutable
      UNION ALL
      SELECT 'confirmed_ledger' AS label, COUNT(*) AS count FROM confirmed_ledger
      UNION ALL
      SELECT 'local_mutation_jobs_unfinished' AS label, COUNT(*) AS count
        FROM local_mutation_jobs WHERE status <> 'completed'
      UNION ALL
      SELECT 'da_payloads' AS label, COUNT(*) AS count FROM da_payloads
    `;
    return new Map(
      rows.map((row) => [row.label, countValue(row.count)] as const),
    );
  }).pipe(Effect.orDie);

export const finalizeE2ESummaryProgram = (
  options: FinalizeSummaryOptions,
): Effect.Effect<FinalizeSummaryResult, never, Database> =>
  Effect.gen(function* () {
    const startedAt = new Date().toISOString();
    const runId = options.runId ?? `e2e-run-${timestampForPath()}`;
    const outDir = options.outDir ?? join("logs", runId);
    const rawLogPath = join(outDir, "collector.log");
    yield* Effect.promise(() => mkdir(outDir, { recursive: true }));

    const nodeUrl = (options.nodeUrl ?? defaultMidgardNodeEndpoint()).replace(
      /\/+$/,
      "",
    );
    const adminHeaders: Readonly<Record<string, string>> =
      options.adminApiKey === undefined || options.adminApiKey.length === 0
        ? {}
        : { "x-midgard-admin-key": options.adminApiKey };
    const stressSummaryPath = options.stressSummaryPath;

    const [readyz, stateQueue, counts, stepSummaries, stressSummary] =
      yield* Effect.all(
        [
          Effect.promise(() => fetchJson(`${nodeUrl}/readyz`)),
          Effect.promise(() =>
            fetchJson(`${nodeUrl}/stateQueue`, adminHeaders),
          ),
          collectDbCounts(),
          Effect.promise(() =>
            loadStepSummaries(options.stepSummaryPaths ?? []),
          ),
          stressSummaryPath === undefined
            ? Effect.succeed(undefined)
            : Effect.promise(() => loadStressSummary(stressSummaryPath)),
        ],
        { concurrency: "unbounded" },
      );
    const finishedAt = new Date().toISOString();

    const pendingUnfinished =
      counts.get("pending_finalizations_unfinished") ?? 0n;
    const volatileResidue =
      (counts.get("mempool") ?? 0n) +
      (counts.get("processed_mempool") ?? 0n) +
      (counts.get("blocks") ?? 0n) +
      (counts.get("local_mutation_jobs_unfinished") ?? 0n);
    const finalized = counts.get("pending_finalizations_finalized") ?? 0n;
    const txBearingHeaders =
      counts.get("pending_finalization_tx_headers") ?? 0n;
    const finalizedTxBearingHeaders =
      counts.get("pending_finalizations_finalized_tx_headers") ?? 0n;
    const consumedDeposits = counts.get("deposits_consumed") ?? 0n;
    const acceptedL2Txs = counts.get("tx_admissions_accepted") ?? 0n;
    const immutableRows = counts.get("immutable") ?? 0n;
    const confirmedLedgerRows = counts.get("confirmed_ledger") ?? 0n;
    const daPayloads = counts.get("da_payloads") ?? 0n;
    const stressEvidence = stressEvidenceFromSummary({
      ...(stressSummary === undefined ? {} : { stressSummary }),
      ...(stressSummaryPath === undefined ? {} : { stressSummaryPath }),
    });
    const transactions = [
      ...(options.transactions ?? []),
      ...stressEvidence.transactions,
    ];
    const expectedL2Count =
      stressSummary === undefined
        ? 2n
        : 2n + BigInt(stressEvidence.acceptedStressCount);
    const allSteps = [
      ...stepSummaries,
      collectorStep({
        startedAt,
        finishedAt,
        rawLogPath,
      }),
    ];
    const requiredFresh = requiredFreshEvidence({
      mode: options.mode ?? "unknown",
      steps: allSteps,
      transactions: options.transactions ?? [],
    });

    const base = createE2ERunSummary({
      runId,
      mode: options.mode ?? "unknown",
      now: new Date(startedAt),
    });
    const summary = updateE2ERunSummary(
      base,
      {
        steps: allSteps,
        transactions,
        http: [
          {
            label: "readyz",
            method: "GET",
            url: `${nodeUrl}/readyz`,
            statusCode: readyz.statusCode,
            semanticStatus:
              readyz.statusCode === 200 && isReady(readyz.body)
                ? "satisfied"
                : "failed",
            source: "e2e-finalize-summary",
          },
          {
            label: "stateQueue",
            method: "GET",
            url: `${nodeUrl}/stateQueue`,
            statusCode: stateQueue.statusCode,
            semanticStatus:
              stateQueue.statusCode === 200 && hasEmptyHeaders(stateQueue.body)
                ? "satisfied"
                : "failed",
            source: "e2e-finalize-summary",
          },
        ],
        db: [
          ...requiredFresh.db,
          {
            label: "finalization_residue",
            status:
              pendingUnfinished === 0n && volatileResidue === 0n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: Object.fromEntries(
              [
                "pending_finalizations_unfinished",
                "mempool",
                "processed_mempool",
                "blocks",
                "local_mutation_jobs_unfinished",
              ].map((label) => [label, (counts.get(label) ?? 0n).toString()]),
            ),
          },
          {
            label: "deposits_consumed",
            status: consumedDeposits === 1n ? "satisfied" : "failed",
            source: "postgres",
            details: {
              consumed: consumedDeposits.toString(),
              expected: "1",
            },
          },
          {
            label: "finalized_headers",
            status:
              finalizedTxBearingHeaders >= txBearingHeaders
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              finalized: finalized.toString(),
              finalizedTxBearing: finalizedTxBearingHeaders.toString(),
              expectedTxBearingMinimum: txBearingHeaders.toString(),
            },
          },
          {
            label: "accepted_l2_txs",
            status:
              (stressSummary === undefined
                ? acceptedL2Txs === expectedL2Count
                : acceptedL2Txs >= expectedL2Count) &&
              immutableRows >= expectedL2Count
                ? "satisfied"
                : "failed",
            source: "postgres",
            details:
              stressSummary === undefined
                ? {
                    accepted: acceptedL2Txs.toString(),
                    expectedAccepted: expectedL2Count.toString(),
                    immutable: immutableRows.toString(),
                    expectedImmutableMinimum: expectedL2Count.toString(),
                  }
                : {
                    accepted: acceptedL2Txs.toString(),
                    expectedAcceptedMinimum: expectedL2Count.toString(),
                    immutable: immutableRows.toString(),
                    expectedImmutableMinimum: expectedL2Count.toString(),
                  },
          },
          {
            label: "confirmed_ledger",
            status:
              confirmedLedgerRows > 0n &&
              daPayloads >= finalized &&
              consumedDeposits === 1n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              confirmedLedger: confirmedLedgerRows.toString(),
              daPayloads: daPayloads.toString(),
              expectedDaPayloadsMinimum: finalized.toString(),
              consumedDeposits: consumedDeposits.toString(),
            },
          },
          ...stressEvidence.db,
        ],
        cleanRunGates: [
          ...requiredFresh.cleanRunGates,
          ...stressEvidence.cleanRunGates,
        ],
        rawEvidence: [
          ...(options.nodeLogPath === undefined
            ? []
            : [{ label: "node-log", path: options.nodeLogPath }]),
          ...stressEvidence.rawEvidence,
        ],
        notes: [
          "Generated by e2e-finalize-summary from live endpoints and database counts.",
          ...stressEvidence.notes,
        ],
      },
      new Date(finishedAt),
    );

    const summaryJsonPath = join(outDir, "summary.json");
    const summaryMarkdownPath = join(outDir, "summary.md");
    yield* Effect.promise(() =>
      writeSummaryJsonAtomic(summaryJsonPath, summary),
    );
    yield* Effect.promise(() =>
      writeSummaryMarkdownAtomic(summaryMarkdownPath, summary),
    );
    return {
      summaryJsonPath,
      summaryMarkdownPath,
      verdict: summary.verdict,
      functionalVerdict: summary.functionalVerdict,
      cleanRunVerdict: summary.cleanRunVerdict,
      nextSafeAction: summary.nextSafeAction,
      requiredFreshStepAttemptQuality: requiredFreshStepAttemptQualityCounts(
        summary.cleanRunGates,
      ),
    };
  });
