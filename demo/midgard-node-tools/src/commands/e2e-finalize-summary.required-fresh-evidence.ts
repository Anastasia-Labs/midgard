import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import type { Database } from "midgard-node/services/database";

import { type StepSummary } from "../e2e/runner.js";
import {
  type CleanRunGate,
  mergeTransactionEvidence,
  type TransactionEvidence,
  transactionEvidenceFromStepSummaries,
} from "../e2e/summary.js";
import {
  type CountRow,
  countValue,
  type FinalizeSummaryOptions,
  type RequiredFreshStepAttemptQualityCounts,
} from "./e2e-finalize-summary.collector-step.js";
import {
  canonicalFreshStepAttemptId,
  canonicalFreshStepSuccessId,
  EVIDENCE_TRANSACTION_STATUSES,
  formatStepAttempts,
  formatTransactions,
  hasReconciledTransactionHash,
  REQUIRED_FRESH_E2E_STEP_ID_SET,
  REQUIRED_FRESH_E2E_STEP_IDS,
  REQUIRED_FRESH_TRANSACTION_LABELS,
  type RequiredFreshEvidenceResult,
  stepHasUnknownInterruptedSubmitRisk,
  stepHasUnresolvedTxObservation,
  UNRESOLVED_TRANSACTION_STATUSES,
} from "./e2e-finalize-summary.stress-evidence-from-summary.js";

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

export const collectDbCounts = (): Effect.Effect<
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
